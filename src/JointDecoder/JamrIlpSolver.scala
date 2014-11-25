package edu.cmu.lti.nlp.amr.JointDecoder

import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.graph.Edge
import edu.cmu.lti.nlp.amr.graph.Edge._
import edu.cmu.lti.nlp.amr.ilp._
import net.sf.javailp
import net.sf.javailp.Constraint

import scala.collection.immutable.IndexedSeq


case class GraphNode(id: String)
case class EdgeWithNodeIdxs(srcIdx: Int, destIdx: Int, label: String)

case class Labeled[T](x: T, label: String)
object Labeled {
  implicit def unwrap[T](labeled: Labeled[T]): T = labeled.x
}

// TODO: Don't need to have edges in concept fragments as vars (but they do need to be scored)
class JamrIlpSolver(val ilpSolver: Solver) {

  def solve(nodeWeights: Seq[Double],
            edgeWeights: Array[Array[Map[String, Double]]],
            exclusions: Iterable[List[Int]],
            mutuallyRequiredNodesAndEdges: List[MutuallyRequired],
            determinismByLabel: collection.Map[String, Int]): (Seq[Int], Seq[EdgeWithNodeIdxs]) = {
    logger(1, "building ILP problem")
    if (nodeWeights.isEmpty) {
        return (Vector(), Seq())
    }
    val realNodes: IndexedSeq[GraphNode] = (0 until nodeWeights.size).map(i => GraphNode(i.toString))
    val dummyRoot = GraphNode("rootedgeObjTerms")
    val dummyEdges: IndexedSeq[Labeled[Edge[GraphNode]]] = realNodes.map(n => Labeled(dummyRoot ~> n, "rootSelector"))
    val realEdges: Array[Array[Array[Labeled[Edge[GraphNode]]]]] =
        edgeWeights.zip(realNodes).map { case (srcWeights, src) =>
            srcWeights.zip(realNodes).map { case (destWeights, dest) =>
                destWeights.toArray.map { case (label, weight) =>
                    Labeled(Edge(src, dest), label)
                }
            }
        }

    /* Variables */
    val nodeVars = realNodes.map(n => BoolVar("n%s".format(n.id))).toSeq
    val realEdgeVars: Array[Array[Array[Labeled[BoolVar]]]] = realEdges.map(_.map(_.map(toBoolVar)))
    val dummyEdgeVars = dummyEdges map toBoolVar
    /* Constraints */
    val simpleGraphConstraints = getSimpleGraphConstraints(nodeVars, realEdgeVars, dummyEdgeVars)
    val (flowVars, flowConstraints) = getSingleCommodityFlowVarsAndConstraints(nodeVars, realEdgeVars, dummyEdgeVars)
    val determinismConstraints = getDeterminismConstraints(nodeVars, realEdgeVars, determinismByLabel) //, labels)
    val exclusionConstraints = getExclusionConstraints(nodeVars, exclusions)
    val mutuallyRequiredConstraints = getMutuallyRequiredConstraints(nodeVars, realEdgeVars, mutuallyRequiredNodesAndEdges)
    val variables = nodeVars ++ realEdgeVars.toSeq.flatten.flatten.map(_.x) ++ dummyEdgeVars.map(_.x) ++ flowVars
    val constraints: Seq[Constraint] = simpleGraphConstraints ++
        flowConstraints ++
        determinismConstraints ++
        exclusionConstraints ++
        mutuallyRequiredConstraints
    /* Objective */
    val objective: Objective = {
        val nodeObjTerms = nodeVars.zip(nodeWeights).collect { case (n, w) if w != 0.0 => n * w }
        val edgeObjTerms = realEdgeVars.toSeq.flatten.flatten.zip(edgeWeights.toSeq.flatten.flatten).collect({
                case(e, (_, w)) if w != 0.0 => e.x * w
            })
        Maximize(Linear(nodeObjTerms.toSeq ++ edgeObjTerms))
    }
    val problem = Problem(variables, objective, constraints)
    // solve it
    logger(1, "sending ILP problem to Gurobi")

    val result = ilpSolver.solve(problem)
    logger(1, "got ILP solution from Gurobi")
    val solnNodes: IndexedSeq[Int] = {
        (0 until nodeWeights.size).zip(nodeVars).collect({ case (i, nodeVar) if result.getBoolean(nodeVar) => i })
    }
    val solnEdges =
        for ((srcEdgeVars, srcIdx) <- realEdgeVars.zipWithIndex;
             (destEdgeVars, destIdx) <- srcEdgeVars.zipWithIndex;
             Labeled(edgeVar, label) <- destEdgeVars
             if result.getBoolean(edgeVar)) yield {
            EdgeWithNodeIdxs(srcIdx, destIdx, label)
        }

    logger(1, s"solnNodes: $solnNodes")
    logger(1, s"solnEdges: $solnEdges")
    (solnNodes, solnEdges)
  }

  def getExclusionConstraints(nodeVars: Seq[BoolVar], exclusions: Iterable[List[Int]]): Iterable[Constraint] = {
      for (nodeIdxs <- exclusions) yield {
          Linear(nodeIdxs.map(nodeVars(_) * 1)) lteq 1
      }
  }

  def getMutuallyRequiredConstraints(realNodeVars: Seq[BoolVar],
                                     realEdgeVars: Array[Array[Array[Labeled[BoolVar]]]],
                                     mutuallyRequiredNodesAndEdges: List[MutuallyRequired]): Seq[Constraint] = {
    val constraints = for (MutuallyRequired(nodeIds, edges) <- mutuallyRequiredNodesAndEdges) yield {
      val equalVars: List[BoolVar] = nodeIds.map(realNodeVars) ++ edges.flatMap({
        case EdgeWithNodeIdxs(srcId, destId, label) => realEdgeVars(srcId)(destId).collect({ case Labeled(e, `label`) => e })
      })
      val representative = equalVars.head
      equalVars.map(v => v minus representative equiv 0)
    }
    constraints.flatten
  }

  protected def toBoolVar(edge: Labeled[Edge[GraphNode]]): Labeled[BoolVar] = {
    Labeled(BoolVar("n%s~%s~>n%s".format(edge.src.id, edge.label, edge.dest.id)), edge.label)
  }

  def getSimpleGraphConstraints(realNodeVars: Seq[BoolVar],
                                realEdgeVars: Array[Array[Array[Labeled[BoolVar]]]],
                                dummyEdgeVars: Seq[Labeled[BoolVar]]): Seq[Constraint] = {
    // An edge may only be included if both its endpoints are included
    val edgeLeEndsConstraints = {
      val realEdgeLeEndsConstraints = realEdgeVars.zipWithIndex flatMap { case (row, srcIdx) =>
        row.zipWithIndex flatMap { case (cell, destIdx) =>
          cell flatMap { edgeVar =>
            Seq(
              realNodeVars(srcIdx) minus edgeVar.x gteq 0,
              realNodeVars(destIdx) minus edgeVar.x gteq 0
            )
          }
        }
      }
      val dummyEdgeLeEndsConstraints = dummyEdgeVars.zipWithIndex map { case (edgeVar, destIdx) =>
        realNodeVars(destIdx) minus edgeVar.x gteq 0
      }
      realEdgeLeEndsConstraints.toSeq ++ dummyEdgeLeEndsConstraints
    }
    // at most one direction can be included
    val simpleConstraints =
      for (srcIdx <- 0 until realNodeVars.size;
           destIdx <- 0 until realNodeVars.size) yield {
        val forwardEdges = Linear(realEdgeVars(srcIdx)(destIdx).map(_ * 1))
        val backwardEdges = Linear(realEdgeVars(destIdx)(srcIdx).map(_ * 1))
        forwardEdges plus backwardEdges lteq 1
      }
    // the root may only have one child
    val singleRootConstraint = Linear(dummyEdgeVars.map(_ * 1)) lteq 1
    edgeLeEndsConstraints.toSeq ++ simpleConstraints :+ singleRootConstraint
  }

  def getDeterminismConstraints(nodeVars: Seq[BoolVar],
                                realEdgeVars: Array[Array[Array[Labeled[BoolVar]]]],
                                determinismByLabel: collection.Map[String, Int]/*,
                                labels: Map[String, Int]*/): Seq[Constraint] = {
    for (outgoingEdges <- realEdgeVars;
         (label, maxOutEdges) <- determinismByLabel/*;
         labelIdx <- labels.get(label)*/) yield {
      Linear(outgoingEdges.toSeq.flatten.collect({ case Labeled(edgeVar, `label`) => edgeVar * 1 })) lteq maxOutEdges
    }
  }

  def getSingleCommodityFlowVarsAndConstraints(nodeVars: Seq[BoolVar],
                                               realEdgeVars: Array[Array[Array[Labeled[BoolVar]]]],
                                               dummyEdgeVars: Seq[Labeled[BoolVar]]): (Seq[IntVar], Seq[Constraint]) = {
    // single-commodity flow variables
    val realFlowVars = realEdgeVars.map(_.map(_.map(e => IntVar(s"flow(${e.x})"))))
    val dummyFlowVars = dummyEdgeVars.map(e => IntVar(s"flow(${e.x})"))
    val flowConstraints: Seq[Constraint] = {
      // The root must send out 1 unit of flow for each node present
      val rootFlowConstraint = Linear(dummyFlowVars.map(_ * 1)) minus Linear(nodeVars.map(_ * 1)) equiv 0
      // Flow may only be sent over an edge if the edge is included in the solution
      val flowLeEdgeConstraints = (for (srcIdx <- 0 until nodeVars.size;
           destIdx <- 0 until nodeVars.size;
           (edgeVar, flowVar) <- realEdgeVars(srcIdx)(destIdx) zip realFlowVars(srcIdx)(destIdx)) yield {
        edgeVar * nodeVars.size minus flowVar gteq 0
      }) ++ (for ((edgeVar, flowVar) <- dummyEdgeVars zip dummyFlowVars) yield {
        edgeVar * nodeVars.size minus flowVar gteq 0
      })
      // TODO: might be better to use gurobi's explicit mechanism for upper/lower bounds on IntVars
      val flowGeZeroConstraints = realFlowVars.flatMap(_.flatMap(_.map(_ * 1 gteq 0))).toSeq ++ dummyFlowVars.map(_ * 1 gteq 0)
      // Each non-root node must consume exactly one unit of flow, if it is included in the solution
      val consumeOneFlowConstraints = nodeVars.zipWithIndex.map { case (nodeVar, i) =>
        val incoming = (dummyFlowVars(i) :: (0 until nodeVars.size).map(srcIdx => realFlowVars(srcIdx)(i)).toList.flatten).map(_ * 1)
        val outgoing = (0 until nodeVars.size).map(destIdx => realFlowVars(i)(destIdx)).flatten.map(_ * -1)
        Linear(incoming ++ outgoing) minus nodeVar equiv 0
      }
      flowGeZeroConstraints ++ flowLeEdgeConstraints ++ consumeOneFlowConstraints :+ rootFlowConstraint
    }
    (realFlowVars.toSeq.flatten.flatten, flowConstraints)
  }
}

object JamrIlpSolver {
  lazy val gurobi = new JamrIlpSolver(Solver(new javailp.SolverFactoryGurobi))
  lazy val cplex = new JamrIlpSolver(Solver(new javailp.SolverFactoryCPLEX))
}
