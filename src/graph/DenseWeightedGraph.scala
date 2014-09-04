package edu.cmu.lti.nlp.amr.graph

import edu.cmu.lti.nlp.amr.util.Weighted
import scala.collection.{mutable => m}
import scala.reflect.ClassTag
import Edge._, Weighted._

/**
* @author sthomson@cs.cmu.edu
*/
class DenseWeightedGraph[V](private val _nodes: Array[V],
                            private val indexOf: collection.Map[V, Int],
                            private val weights: Array[Array[Double]]) extends WeightedGraph[V] {
  override val nodes = _nodes.toSet

  override def edge(src: V, dest: V): Option[Weighted[Edge[V]]] = {
    for (i <- indexOf.get(src);
         j <- indexOf.get(dest);
         weight = weights(i)(j)
         if weight != Double.NegativeInfinity) yield src ~> dest % weight
  }

  override def outgoingEdges(src: V): Iterable[Weighted[Edge[V]]] = {
    if (!indexOf.contains(src)) return Set()
    val srcIdx = indexOf(src)
    for ((dest, weight) <- _nodes zip weights(srcIdx)
         if weight != Double.NegativeInfinity) yield {
      src ~> dest % weight
    }
  }

  override def incomingEdges(dest: V): Iterable[Weighted[Edge[V]]] = {
    if (!indexOf.contains(dest)) return Set()
    val destIdx = indexOf(dest)
    for ((src, srcWeights) <- _nodes zip weights;
         weight = srcWeights(destIdx)
         if weight != Double.NegativeInfinity) yield {
      src ~> dest % weight
    }
  }

  override def edges: Iterable[Weighted[Edge[V]]] = {
    for ((src, srcWeights) <- _nodes zip weights;
         (dest, weight) <- _nodes zip srcWeights
         if weight != Double.NegativeInfinity) yield {
      src ~> dest % weight
    }
  }
}

object DenseWeightedGraph {
  def apply[V : ClassTag](nodes: Iterable[V], weights: Array[Array[Double]]): DenseWeightedGraph[V] = {
    val nodeList = nodes.toArray
    require(nodeList.size == weights.length)
    val indexOf = m.Map() ++ nodeList.zipWithIndex
    new DenseWeightedGraph[V](nodeList, indexOf, weights)
  }

  def apply(weights: Array[Array[Double]]): DenseWeightedGraph[Int] = {
    DenseWeightedGraph(0 until weights.length, weights)
  }
}
