package edu.cmu.lti.nlp.amr.graph

import scala.collection.{mutable => m}
import edu.cmu.lti.nlp.amr.util.{DefaultDict, Weighted}

/**
* @author sthomson@cs.cmu.edu
*/
class SparseWeightedGraph[V](_nodes: Iterable[V], _edges: Iterable[Weighted[Edge[V]]]) extends WeightedGraph[V] {
  private lazy val _incomingEdges = {
    val inEdges = DefaultDict((_: V) => m.Map[V, Weighted[Edge[V]]]())
    edges.foreach(e => inEdges(e.dest) += e.src -> e)
    inEdges.mapValues(_.toMap).toMap
  }
  private lazy val _outgoingEdges = {
    val outEdges = DefaultDict((_: V) => m.Map[V, Weighted[Edge[V]]]())
    edges.foreach(e => outEdges(e.src) += e.dest -> e)
    outEdges.mapValues(_.toMap).toMap
  }

  override lazy val nodes: Set[V] = _nodes.toSet

  override lazy val edges: Set[Weighted[Edge[V]]] = _edges.filter(!_.isAutoCycle).toSet

  override def edge(src: V, dest: V): Option[Weighted[Edge[V]]] = _incomingEdges.get(dest).flatMap(_.get(src))

  override def outgoingEdges(src: V): Iterable[Weighted[Edge[V]]] = _outgoingEdges.getOrElse(src, Map()).values

  override def incomingEdges(dest: V): Iterable[Weighted[Edge[V]]] = _incomingEdges.getOrElse(dest, Map()).values
}

object SparseWeightedGraph {
  def apply[V](nodes: Iterable[V], edges: Iterable[Weighted[Edge[V]]]): SparseWeightedGraph[V] = {
    new SparseWeightedGraph[V](nodes, edges)
  }
  def apply[V](edges: Weighted[Edge[V]]*): SparseWeightedGraph[V] = {
    SparseWeightedGraph(edges.map(_.src) ++ edges.map(_.dest), edges)
  }
}
