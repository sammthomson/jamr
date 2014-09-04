package edu.cmu.lti.nlp.amr.graph

import edu.cmu.lti.nlp.amr.util.Weighted

/** An edge in a directed graph. */
case class Edge[T](src: T, dest: T) {
  lazy val isAutoCycle: Boolean = src == dest

  override def toString: String = s"$src ~> $dest"
}

object Edge {
  // sugar
  val ~> = Edge  // for unapply / pattern matching
  case class EdgeAssoc[T](private val _src: T) {
    def ~> (dest: T): Edge[T] = Edge(_src, dest)
  }
  implicit def any2EdgeAssoc[T](src: T): EdgeAssoc[T] = EdgeAssoc(src)
}

/**
 * A Weighted, directed graph with node type `V`.
 * Autocycles (`x ~> x`) are not allowed.
 *
 * @author sthomson@cs.cmu.edu
 */
abstract class WeightedGraph[V] {
  def nodes: Iterable[V]

  def edges: Iterable[Weighted[Edge[V]]]

  def outgoingEdges(src: V): Iterable[Weighted[Edge[V]]]

  def incomingEdges(dest: V): Iterable[Weighted[Edge[V]]]

  def edge(src: V, dest: V): Option[Weighted[Edge[V]]]

  def weightOf(src: V, dest: V): Option[Double] = edge(src, dest).map(_.weight)

  def filter(predicate: Edge[V] => Boolean): WeightedGraph[V] = {
    SparseWeightedGraph(nodes, edges.filter(predicate(_)))
  }

  override def toString: String = s"WeightedGraph(\n\tnodes=${nodes.mkString(", ")},\n\tedges=${edges.mkString(", ")}\n)"
}
