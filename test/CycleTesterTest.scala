package edu.cmu.lti.nlp.amr

import org.scalatest.{FlatSpec, Matchers}
import CycleTester.hasCycle

/**
 * @author sthomson@cs.cmu.edu
 */
class CycleTesterTest extends FlatSpec with Matchers {
    "CycleTester" should "detect a 3-node cycle" in {
        val graphA = Map(
            1 -> Set(2),
            2 -> Set(3),
            3 -> Set(1)
        )
        hasCycle(1 to 3, graphA) should be (true)
    }

    it should "not detect a cycle in a DAG with a reentrancy" in {
        val graphB = Map(
            1 -> Set(2, 3),
            2 -> Set(3)
        )
        hasCycle(1 to 3, graphB) should be (false)
    }

    it should "not detect a cycle in a DAG with two reentrancies" in {
        val graphC = Map(
            1 -> Set(2),
            2 -> Set(3, 4),
            4 -> Set(5, 6),
            5 -> Set(6),
            6 -> Set(3)
        )
        hasCycle(1 to 6, graphC) should be (false)
    }

    it should "detect the cycle 4->5->6->4" in {
        val graphD = Map(
            1 -> Set(2),
            2 -> Set(3, 4),
            4 -> Set(5),
            5 -> Set(6),
            6 -> Set(3, 4)
        )
        hasCycle(1 to 6, graphD) should be (true)
    }
}
