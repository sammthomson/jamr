package edu.cmu.lti.nlp.amr.JointDecoder

import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair
import edu.cmu.lti.nlp.amr.FastFeatureVector._
import edu.cmu.lti.nlp.amr.GraphDecoder.GraphObj
import edu.cmu.lti.nlp.amr.{Node => gNode, _}

import scala.collection.{mutable => m}


case class CandidateFragment(concepts: PhraseConceptPair,
                             span: Span,
                             edges: List[(String, String, String)],
                             score: Double)

class IlpDecoder(stage1FeatureNames: List[String],
                 phraseConceptPairs: Array[ConceptInvoke.PhraseConceptPair],  // this is the concept table
                 stage2FeatureNames: List[String],
                 labelSet: Array[(String, Int)]) extends Decoder {
  // these are the weights that will be updated during training
  // these include both stage1 and stage2 weights
  val weights = FeatureVector(labelSet.map(_._1))
  // we ignore stage1Features.weights, and only put all the weights in stage2Features.weights
  val stage1Features = new ConceptInvoke.Features(stage1FeatureNames)
  val stage2Features = new GraphDecoder.Features(stage2FeatureNames, weights.labelset)

  val conceptInvoker = new ConceptInvoke.Concepts(phraseConceptPairs)

  val ilpSolver = JamrIlpSolver.gurobi

  def decode(input: Input) : FastFeatureVector.DecoderResult = {
    stage2Features.input = input
    stage2Features.weights = weights
    val sentence = input.sentence
    var feats = new FeatureVector(weights.labelset)

    val (fullGraph, candidateFragments) = getCandidateFragments(input)

    val scoresByRootNode: Map[String, Double] =
      candidateFragments.map(fragment => fragment.span.nodeIds.head -> fragment.score).toMap


    assert(fullGraph.nodes.forall(_.name != None), "all nodes should have a name: " + fullGraph.nodes.filterNot(_.name != None))
    val candidateNodes: Array[gNode] = fullGraph.nodes.toArray.sortBy(_.id)
    val indexByNodeId: Map[String, Int] = candidateNodes.zipWithIndex.map({case (node, i) => node.id -> i}).toMap
    logger(1, s"indexByNodeId: $indexByNodeId")
    // map from token idx to list of Node indexes, only one of which may be in the final solution
    val exclusionsByTokenId = m.Map[Int, List[Int]]()
    for (fragment <- candidateFragments;
         i <- fragment.span.start until fragment.span.end) {
      exclusionsByTokenId += i -> (indexByNodeId(fragment.span.nodeIds.head) :: exclusionsByTokenId.getOrElse(i, Nil))
    }
    val mutuallyRequiredNodesAndEdges: List[(List[Int], List[(Int, Int, String)])] = for (fragment <- candidateFragments) yield {
      val edges: List[(Int, Int, String)] = fragment.edges.map { case (srcId, label, destId) => (indexByNodeId(srcId), indexByNodeId(destId), label)}
      (fragment.span.nodeIds.map(indexByNodeId), edges)
    }
    val nodeWeights: Array[Double] = candidateNodes.map(node => scoresByRootNode.getOrElse(node.id, 0.0))
    val edgeWeights: Array[Array[Array[(String, Double)]]] = new GraphObj(fullGraph, candidateNodes, stage2Features).edgeWeights // TODO: don't need to score edges between mutually exclusive nodes

    val candidateFragmentByNode = Map() ++ (for (fragment <- candidateFragments; nodeId <- fragment.span.nodeIds.headOption) yield { nodeId -> fragment })

    logger(1, "nodeById: " + fullGraph.getNodeById)
    logger(1, "nodeWeights: " + nodeWeights.toList)
    logger(1, "edgeWeights: " + edgeWeights.toList.map(_.toList.map(_.toList)))
    val (nodesInSolution, edgesInSolution) = ilpSolver.solve(
      nodeWeights,
      edgeWeights,
      exclusionsByTokenId,
      mutuallyRequiredNodesAndEdges,
      labelSet.toMap
    )

    logger(1, "Nodes in solution: " + nodesInSolution)
    logger(1, "Edges in solution: " + edgesInSolution)

    var fragmentsToAdd = List[CandidateFragment]() // TODO
    for (node <- nodesInSolution) {
      if (candidateFragmentByNode.contains(node.id)) {
        fragmentsToAdd = candidateFragmentByNode(node.id) :: fragmentsToAdd
      }
    }

    // edgesToAdd can't include the edges from the CandidateConcepts
    var edgesToAdd = List[(String, String, String)]() // TODO
    for (edge <- edgesInSolution) {
      edgesToAdd = (edge.src.id, edge.label, edge.dest.id) :: edgesToAdd
    }

    var graph: Graph = Graph.empty()
    graph.getNodeById.clear()
    graph.getNodeByName.clear()
    for (fragment <- fragmentsToAdd) {
      graph.addNewSpan(fragment.span, fullGraph)
      val localFeatures = stage1Features.localFeatures(input, fragment.concepts, fragment.span.start, fragment.span.end)
      feats += FastFeatureVector.fromBasicFeatureVector(localFeatures, weights.labelset)
    }

    for ((srcId, relation, destId) <- edgesToAdd) {
      val srcNode = graph.getNodeById(srcId)
      val destNode = graph.getNodeById(destId)
      srcNode.relations = (relation, destNode) :: srcNode.relations
      feats += stage2Features.localFeatures(srcNode, destNode, relation)
    }

    val nodes = graph.nodes.toArray
    if(nodes.size > 0) {
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

    DecoderResult(graph, feats, weights.dot(feats))
  }

  def getCandidateFragments(input: Input) : (Graph, List[CandidateFragment]) = {
    logger(1, "\n--- getting candidate concepts ---\n")
    val sentence = input.sentence
    var candidateConcepts = List[CandidateFragment]()
    val graph = Graph.empty
    graph.getNodeById.clear()
    graph.getNodeByName.clear()
    for (i <- Range(0, sentence.size)) {
      logger(2, "word = "+sentence(i))
      val conceptList = conceptInvoker.invoke(input, i)
      logger(2, "Possible invoked concepts: " + conceptList)
      // WARNING: the code below assumes that anything in the conceptList will not extend beyond the end of the sentence (and it shouldn't based on the code in Concepts)
      for (concept <- conceptList) {
        val end = i + concept.words.size
//        val score = stage1Features.localScore(input, concept, i, end)
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
    (graph, candidateConcepts)
  }
}
