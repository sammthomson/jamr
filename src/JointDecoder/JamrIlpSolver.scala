package JointDecoder

import net.sf.javailp
import net.sf.javailp.Constraint
import edu.cmu.lti.nlp.amr.ilp._
import edu.cmu.lti.nlp.amr.util.Weighted

import scala.collection.Map


case class Node(id: String)


class JamrIlpSolver(val ilpSolver: Solver) {

  def solve(nodeWeights: Array[Double],
            edgeWeights: Array[Array[Array[(String, Double)]]],
            exclusionsByTokenId: Map[Int, List[Int]],
            mutuallyRequiredNodesAndEdges: List[(List[Int], List[(Int, Int, String)])]) = {
    val nodes = graph.nodes.toSeq
    val dummyRoot = Node("root") % 0
    val dummyEdges = nodes.map(n => n -> (dummyRoot ~> n % 0)).toMap
    val edges = graph.edges.toSeq ++ dummyEdges.values

    /* Variables */
    val nodeVars = nodes.map(n => n -> BoolVar("n%s".format(n.id))).toMap
    val edgeVars = edges.map(e => e -> BoolVar("n%s~>n%s".format(e.src.id, e.dest.id))).toMap

    /* Constraints */
    val simpleGraphConstraints = getSimpleGraphConstraints(graph, dummyEdges, nodeVars, edgeVars)
    val (flowVars, flowConstraints) = getSingleCommodityFlowVarsAndConstraints(graph, dummyEdges, nodeVars, edgeVars)

    val variables = nodeVars.values ++ edgeVars.values ++ flowVars
//    val asymmConstraints = asymmetryConstraints(dummyEdges, nodeVars, edgeVars)
    val constraints = simpleGraphConstraints ++ flowConstraints  // ++ asymmConstraints
    /* Objective */
    val objective = {
      val nodeObjTerms = nodeVars collect { case (node, nodeVar) if node.weight != 0.0 => nodeVar * node.weight }
      val edgeObjTerms = edgeVars collect { case (edge, edgeVar) if edge.weight != 0.0 => edgeVar * edge.weight }
      Maximize((nodeObjTerms.toSeq ++ edgeObjTerms).reduce(plus))
    }
    val problem = Problem(variables, objective, constraints)
    // solve it
    val result = ilpSolver.solve(problem)
    val solnNodes = {
      nodes.filter(n => result.getBoolean(nodeVars(n)))
    }
    val solnEdges = edges.filter(e => e.src != dummyRoot && result.getBoolean(edgeVars(e)))
    SparseWeightedGraph(solnNodes, solnEdges)
  }

  def getSimpleGraphConstraints(graph: G,
                                dummyEdges: Map[N, Weighted[Edge[Weighted[Node]]]],
                                nodeVars: Map[N, BoolVar],
                                edgeVars: Map[Weighted[Edge[Weighted[Node]]], BoolVar]): Seq[Constraint] = {
    val nodes = nodeVars.keys.toSeq
    // An edge may only be included if both its endpoints are included
    val edgeLeEndsConstraints = {
      val edgeLeSrcConstraints =
        for ((edge, edgeVar) <- edgeVars;
             srcNodeVar <- nodeVars.get(edge.src)) yield {  // nodeVars does not include dummy root
          srcNodeVar minus edgeVar gteq 0
        }
      val edgeLeDestConstraints =
        for ((edge, edgeVar) <- edgeVars;
             destNodeVar <- nodeVars.get(edge.dest)) yield {
          destNodeVar minus edgeVar gteq 0
        }
      edgeLeSrcConstraints ++ edgeLeDestConstraints
    }
    // at most one direction can be included
    val simpleConstraints =
      for (src <- nodes;
           dest <- nodes;
           fwdEdge <- graph.edge(src, dest).map(edgeVars);
           bkwdEdge <- graph.edge(dest, src).map(edgeVars)) yield {
        fwdEdge plus bkwdEdge lteq 1
      }
    // the root may only have one child
    val singleRootConstraint = dummyEdges.values.map(edgeVars(_) * 1).reduce(plus) lteq 1
    edgeLeEndsConstraints.toSeq ++ simpleConstraints :+ singleRootConstraint
  }

  @deprecated("slower than without")
  def getAsymmetryConstraints(dummyEdges: Map[N, Weighted[Edge[Weighted[Node]]]],
                              nodeVars: Map[N, BoolVar],
                              edgeVars: Map[Weighted[Edge[Weighted[Node]]], BoolVar]): Seq[Constraint] = {
    val nodes = nodeVars.keys.toSeq
    for ((u, i) <- nodes.zipWithIndex;
         v <- nodes.drop(i + 1)) yield {
      nodeVars(u) plus edgeVars(dummyEdges(v)) lteq 1
    }
  }

  def getSingleCommodityFlowVarsAndConstraints(graph: G,
                                               dummyEdges: Map[N, Weighted[Edge[Weighted[Node]]]],
                                               nodeVars: Map[N, BoolVar],
                                               edgeVars: Map[Weighted[Edge[N]], BoolVar]): (Seq[IntVar], Seq[Constraint]) = {
    val nodes = nodeVars.keys.toSeq
    val edges = edgeVars.keys.toSeq
    // single-commodity flow variables
    val flowVars: Map[Weighted[Edge[N]], IntVar] = edges.map(e => e -> IntVar(s"flow(${edgeVars(e)})")).toMap
    val flowConstraints: Seq[Constraint] = {
      // The root must send out 1 unit of flow for each node present
      val rootFlowConstraint = {
        dummyEdges.values.map(flowVars(_) * 1).reduce(plus) minus nodeVars.values.map(_ * 1).reduce(plus) equiv 0
      }
      // Flow may only be sent over an edge if the edge is included in the solution
      val flowLeEdgeConstraints = edges.map(e => edgeVars(e) * nodes.size minus flowVars(e) gteq 0)
      // TODO: might be better to use gurobi's explicit mechanism for upper/lower bounds on IntVars
      val flowGeZeroConstraints = flowVars.values.map(_ gteq 0).toSeq
      // Each non-root node must consume exactly one unit of flow, if it is included in the solution
      val consumeOneFlowConstraints = nodes.map { n =>
        val incoming = (dummyEdges(n) :: graph.incomingEdges(n).toList).map(flowVars(_) * 1)
        val outgoing = graph.outgoingEdges(n).toList.map(flowVars(_) * -1)
        (incoming ++ outgoing).reduce(plus) minus nodeVars(n) equiv 0
      }
      flowGeZeroConstraints ++ flowLeEdgeConstraints ++ consumeOneFlowConstraints :+ rootFlowConstraint
    }
    (flowVars.values.toSeq, flowConstraints)
  }

  @deprecated("slower than single-commodity")
  def getMultiCommodityFlowVarsAndConstraints(graph: G,
                                              dummyEdges: Map[N, Weighted[Edge[Weighted[Node]]]],
                                              nodeVars: Map[N, BoolVar],
                                              edgeVars: Map[Weighted[Edge[N]], BoolVar]): (Seq[BoolVar], Seq[Constraint]) = {
    val nodes = nodeVars.keys.toSeq
    val edges = edgeVars.keys.toSeq
    // multi-commodity flow variables
    val flowVars = nodes.map(n => n -> edges.map(e => e -> BoolVar(s"flow_${n.id}(${edgeVars(e)})")).toMap).toMap
    val flowConstraints = {
      // The root must send out one unit of flow to each node that is present
      val rootFlowConstraints = nodes map { flowType =>
        dummyEdges.values.map(e => flowVars(flowType)(e) * 1).reduce(plus) minus nodeVars(flowType) equiv 0
      }
      // Flow may only be sent over an edge if the edge is included in the solution
      val flowLeEdgeConstraints = nodes flatMap { flowType => edges map { e =>
        edgeVars(e) minus flowVars(flowType)(e) gteq 0
      } }
      // Each non-root node present must consume 1 unit of its own flow, and be balanced for other flows
      val flowBalanceConstraints = nodes flatMap { flowType =>
        val flows = flowVars(flowType)
        nodes map { n =>
          val incoming = (dummyEdges(n) :: graph.incomingEdges(n).toList).map(flows(_) * 1)
          val outgoing = graph.outgoingEdges(n).map(flows(_) * -1)
          if (flowType == n) {
            (incoming ++ outgoing).reduce(plus) minus nodeVars(n) equiv 0
          } else {
            (incoming ++ outgoing).reduce(plus) equiv 0
          }
        }
      }
      flowLeEdgeConstraints ++ flowBalanceConstraints ++ rootFlowConstraints
    }
    (flowVars.values.flatMap(_.values).toSeq, flowConstraints)
  }
}

object JamrIlpSolver {
  private type N = Weighted[Node]
  private type E = Weighted[Edge[N]]
  private type G = WeightedGraph[N]

  lazy val gurobi = new JamrIlpSolver(Solver(new javailp.SolverFactoryGurobi))
  lazy val cplex = new JamrIlpSolver(Solver(new javailp.SolverFactoryCPLEX))
}
