package edu.cmu.lti.nlp.amr.JointDecoder

import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair
import edu.cmu.lti.nlp.amr.FastFeatureVector._
import edu.cmu.lti.nlp.amr.GraphDecoder.Features
import edu.cmu.lti.nlp.amr._

import scala.collection.{mutable => m}


case class CandidateFragment(concepts: PhraseConceptPair,
                             span: Span,
                             edges: List[(String, String, String)],
                             score: Double)

/** nodes and edges that must either all be in the solution, or none be in the solution */
case class MutuallyRequired(nodeIds: List[Int], edges: List[EdgeWithNodeIdxs])


/**
 * JointDecoder.Decoder that works by calling an external ILP solver like Gurobi or CPLEX
 */
class IlpDecoder(stage1FeatureNames: List[String],
                 phraseConceptPairs: Array[ConceptInvoke.PhraseConceptPair], // this is the concept table
                 stage2FeatureNames: List[String],
                 labelSet: Array[(String, Int)]) extends Decoder {
    // these are the weights that will be updated during training
    // these include both stage1 and stage2 weights
    var weights = FeatureVector(labelSet.map(_._1))
    // we ignore stage1Features.weights, and only put all the weights in stage2Features.weights
    val stage1Features = new ConceptInvoke.Features(stage1FeatureNames)
    val stage2Features = new GraphDecoder.Features(stage2FeatureNames, weights.labelset)

    val conceptInvoker = new ConceptInvoke.Concepts(phraseConceptPairs)

    val ilpSolver = JamrIlpSolver.gurobi

    def decode(input: Input): FastFeatureVector.DecoderResult = {
        logger(1, s"begin joint decoding ${input.sentence.mkString(" ")}")
        stage2Features.input = input
        stage2Features.weights = weights
        var feats = new FeatureVector(weights.labelset)

        val (fullGraph, candidateFragments) = getCandidateFragments(input)

        stage2Features.graph = fullGraph

        val candidateNodes: Array[Node] = fullGraph.nodes.toArray
        val indexByNodeId: Map[String, Int] = candidateNodes.zipWithIndex.map({ case (node, i) => node.id -> i}).toMap
        val scoresByRootNodeId: Map[String, Double] =
            candidateFragments.map(fragment => fragment.span.nodeIds.head -> fragment.score).toMap
        val nodeWeights: Array[Double] = candidateNodes.map(node => scoresByRootNodeId.getOrElse(node.id, 0.0))

        val candidateFragmentByNodeIdx: Map[Int, CandidateFragment] = Map() ++ (
            for (fragment <- candidateFragments;
                 nodeId <- fragment.span.nodeIds.headOption) yield {
                indexByNodeId(nodeId) -> fragment
            })

        // only one in each List of Node indexes may be in the final solution
        val exclusions: Seq[List[Int]] = {
            val excls = m.Map[Int, List[Int]]()
            for (fragment <- candidateFragments;
                 i <- fragment.span.start until fragment.span.end) {
                excls += i -> (indexByNodeId(fragment.span.nodeIds.head) :: excls.getOrElse(i, Nil))
            }
            excls filter { case (k, v) => v.size > 1 }  // only need to add a constraint if there is a conflict
        }.values.toSeq
        // list of (nodeIds, edgeIds) sets
        val mutuallyRequiredNodesAndEdges: List[MutuallyRequired] =
            candidateFragments.map(fragment => {
                val edges = fragment.edges map { case (srcId, label, destId) =>
                    EdgeWithNodeIdxs(indexByNodeId(srcId), indexByNodeId(destId), label)
                }
                MutuallyRequired(fragment.span.nodeIds.map(indexByNodeId), edges)
            }).filter {
                // only need a constraint if there are more than one nodes or edges involved
                case MutuallyRequired(ns, edges) => ns.size + edges.size > 1
            }

        val edgeWeights: Array[Array[Map[String, Double]]] =
            scoreEdges(fullGraph, candidateNodes, stage2Features)

        val (nodesInSolution, edgesInSolution) = ilpSolver.solve(
            nodeWeights,
            edgeWeights,
            exclusions,
            mutuallyRequiredNodesAndEdges,
            labelSet.toMap
        )

        val fragmentsToAdd = nodesInSolution.flatMap(candidateFragmentByNodeIdx.get)
        val edgesInCandidateFragments = fragmentsToAdd.flatMap(_.edges).toSet

        val edgesToAdd = edgesInSolution.iterator.map(edge =>
            (candidateNodes(edge.srcIdx).id, edge.label, candidateNodes(edge.destIdx).id)
        ).filter(edge => !edgesInCandidateFragments.contains(edge)).seq

        logger(1, s"fragmentsToAdd: $fragmentsToAdd")
        logger(1, s"edgesToAdd: $edgesToAdd")
        logger(1, s"fullGraph nodes: ${fullGraph.getNodeById}")

        var graph: Graph = Graph.empty()
        graph.getNodeById.clear()
        graph.getNodeByName.clear()
        stage2Features.graph = graph
        for (fragment <- fragmentsToAdd) {
            graph.addNewSpan(fragment.span, fullGraph)
            val localFeatures = stage1Features.localFeatures(input, fragment.concepts, fragment.span.start, fragment.span.end)
            feats += FastFeatureVector.fromBasicFeatureVector(localFeatures, weights.labelset)
        }

        //    logger(1, s"graph.getNodeById ${graph.getNodeById}")
        for ((srcId, relation, destId) <- edgesToAdd) {
            val srcNode = graph.getNodeById(srcId)
            val destNode = graph.getNodeById(destId)
            // TODO: don't we have to clear relations for all the nodes before we do this?
            srcNode.relations = (relation, destNode) :: srcNode.relations //.toSet.toList // so we don't add edges in candidate fragments twice. TODO: inefficient
            feats += stage2Features.localFeatures(srcNode, destNode, relation)
        }

        val nodes = graph.nodes.toArray
        if (nodes.size > 0) {
            if (stage2Features.rootFeatureFunctions.size != 0 && nodes.count(node => !node.concept.startsWith("\"") && !node.concept.matches("[0-9].*")) != 0) {
                graph.root = nodes.filter(node => !node.concept.startsWith("\"") && !node.concept.matches("[0-9].*")).map(x => (x, stage2Features.rootScore(x))).maxBy(_._2)._1
            } else {
                //logger(1, "Setting root to "+nodes(0).id)
                graph.root = nodes.head
            }
            feats += stage2Features.rootFeatures(graph.root)
            graph.makeTopologicalOrdering()
        } else {
            graph = Graph.empty()
        }
        logger(1, s"finished joint decoding ${input.sentence.mkString(" ")}")
        DecoderResult(graph, feats, weights.dot(feats))
    }

    def getCandidateFragments(input: Input): (Graph, List[CandidateFragment]) = {
        logger(1, "\n--- getting candidate concepts ---\n")
        val sentence = input.sentence
        var candidateConcepts = List[CandidateFragment]()
        val graph = Graph.empty()
        graph.getNodeById.clear()
        graph.getNodeByName.clear()
        for (i <- Range(0, sentence.size)) {
            logger(2, "word = " + sentence(i))
            val conceptList = conceptInvoker.invoke(input, i)
            logger(2, "Possible invoked concepts: " + conceptList)
            for (concept <- conceptList) {
                val end = i + concept.words.size
                val score = {
                    val localFeats = stage1Features.localFeatures(input, concept, i, end)
                    stage2Features.weights.dot(FastFeatureVector.fromBasicFeatureVector(localFeats, stage2Features.weights.labelset))
                }
                logger(1, "concept = " + concept.toString)
                logger(2, "score = " + score.toInt)
                val (edges, span) = graph.addSpan(sentence, i, end, concept.graphFrag)
                candidateConcepts = CandidateFragment(concept, span, edges, score) :: candidateConcepts
            }
        }
        graph.normalizeInverseRelations()
        (graph, candidateConcepts)
    }

    /**
     * a rule-based pruner so we don't have to consider every label for every edge
     * - Constants can't have outgoing labels, can only have incoming ":op"s?
     * - "name"s can only have incoming ":name" and outgoing ":op"s
     * - PropBank XX-01 style concepts won't have ":op" or ":name"
     * - things that don't end in "-01" might not have ":arg"s
     * - "and"s and "or"s can only have ":ops"
     * - "-" can only have ":polarity" incoming
     */
    def validEdgeLabel(label: String, src: Node, dest: Node): Boolean = {
        if (src.concept == "name") {
            label == ":op"
        } else if (dest.concept == "name") {
            label == ":name"
        } else if (label == ":name") {
            false
        } else if (dest.concept == "-") {
            label == ":polarity"
        } else if (src.isConstant) {
            false
        } else if (dest.isConstant) {
            label == ":op"
        } else if (src.takesArgs) {
            !Seq(":op", ":name").contains(label)
        } else if (label.startsWith(":ARG")) {
            false
        } else if (src.takesOps) {
            label.startsWith(":op")
        } else {
            true
        }
    }

    /** TODO:
      * Don't need to score edges that compete with concept fragments
      * Don't need to score edges between concept fragments that overlap
      */
    def scoreEdges(graph: Graph,
                   nodes: Array[Node],
                   features: Features): Array[Array[Map[String, Double]]] = {
        for (srcIdx <- (0 until nodes.size).toArray) yield {
            for (destIdx <- (0 until nodes.size).toArray) yield {
                if (srcIdx == destIdx) {
                    Map.empty[String, Double]
                } else {
                    val src = nodes(srcIdx)
                    val dest = nodes(destIdx)
                    // TODO: would be nice to prune before we calculate features
                    val feats = features.localFeatures(src, dest)
                    val edgeScores = for(
                        Conjoined(labelIdx, value) <- features.weights.conjoinedScores(feats);
                        label = features.weights.labelset(labelIdx)
                        if validEdgeLabel(label, src, dest)
                    ) yield {
                        label -> value
                    }
                    edgeScores.toMap
                }
            }
        }
    }
}
