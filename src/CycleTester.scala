// Copyright (c) 2014, Sam Thomson
package edu.cmu.lti.nlp.amr

import scala.annotation.tailrec

object CycleTester {
  /**
    Takes a directed graph, as a set of nodes and a map from node to its out-adjacent nodes,
    and determines whether the graph contains a cycle.
   */
  @tailrec
  def hasCycle[T](nodes: Traversable[T], outgoingEdges: Map[T, Traversable[T]]): Boolean = {
    // Iteratively remove leaves.
    // When there are no leaves left, if there are still nodes left then there is a cycle.
    if (outgoingEdges.isEmpty) {
      false
    } else {
      val oFirstLeaf: Option[T] = nodes.toIterator.find(v => outgoingEdges.getOrElse(v, Nil).isEmpty).headOption
      oFirstLeaf match {
        case None => true
        case Some(leaf) =>
            // remove `leaf` from the graph and continue
            val removed = (outgoingEdges - leaf) map { case (k, v) => (k, v.toSet - leaf) }
            hasCycle(nodes.toSet - leaf, removed)
      }
    }
  }
}
