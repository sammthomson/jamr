package JointDecoder


import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair
import edu.cmu.lti.nlp.amr.FastFeatureVector.{Conjoined, ValuesList}
import edu.cmu.lti.nlp.amr._
import org.scalatest.Matchers

import scala.Double.{NegativeInfinity => minusInfty}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m}


/**
* @author sthomson@cs.cmu.edu
*/
class IlpDecoderTest extends org.scalatest.FlatSpec with Matchers {

    def node(name: String) : Node = {
      return Node(name, Some(name), name, List(), List(), List(), None, ArrayBuffer())
    }

    def weights(weightList : List[(String, String, String, Double)], labelSet: Array[String]): FastFeatureVector.FeatureVector = {
      val feats = FastFeatureVector.FeatureVector(labelSet)
      for ((node1, node2, relation, weight) <- weightList) {
        feats += List(("CA:Id1="+node1+"+Id2="+node2, ValuesList(0.0, List(Conjoined(feats.labelToIndex(relation), weight)))))
      }
      feats
    }

    "IlpDecoder" should "get a small graph correct" in {
      verbosity = 1
      val sentence = Array("a", "b", "c")
      val determinismByLabel = Array((":r", 1))
      val decoder = new JointDecoder.IlpDecoder(
        List("conceptGivenPhrase"),
        Array(
          new PhraseConceptPair("a ||| a-01 ||| c|p=100"),
          new PhraseConceptPair("b ||| b-01 ||| c|p=-1"),
          new PhraseConceptPair("c ||| c-01 ||| c|p=20")
        ),
        List("CostAugEdgeId"),
        determinismByLabel
      )
      val labelSet = determinismByLabel.map(_._1)
      decoder.weights += FastFeatureVector.fromBasicFeatureVector(BasicFeatureVector.FeatureVector(m.Map("c|p" -> 1.0)), labelSet)
      decoder.weights += weights(
        List(
          ("0", "1", ":r", 6.0),
          ("1", "2", ":r", 5.0),
          ("0", "2", ":r", 1.0)
        ),
        labelSet
      )
      logger(1, decoder.stage2Features.weights)
      val result = decoder.decode(Input(
        None,
        sentence,
        Annotation(sentence, sentence, sentence),
        Annotation(sentence, sentence, sentence.zipWithIndex.map({case (tok, i) => Dependency(-1, i, "nsubj")})),
        Annotation(sentence, sentence, sentence),
        Annotation(sentence, sentence, Array())
      ))
      logger(1, "Triples:")
      logger(1, result.graph.printTriples(detail = 1))
    }

//    def test2() {
//      println("Test2")
//      val nodes = Map("1" -> node("1"),
//        "2" -> node("2"),
//        "3" -> node("3"))
//      val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
//      val decoder = new Alg2(List("edgeId"), Array((":r", 1)))
//      decoder.features.weights = weights(
//        List(("1", "2", ":r", 6),
//          ("2", "3", ":r", -6),
//          ("1", "3", ":r", 3)))
//      println("weights:")
//      print(decoder.features.weights)
//      val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
//      println("In test2()")
//      for (node <- result.graph.nodes) { println(node.topologicalOrdering.map(x => (x._1, x._2.id))) }
//      println("Triples:")
//      result.graph.printTriples(detail = 1)
//      println("TopologicalOrdering:")
//      for (node <- result.graph.nodes) {
//        println(node.id + "=" + node.topologicalOrdering.map(x => (x._1, x._2.id)))
//      }
//      println("Graph:")
//      println(result.graph.root.prettyString(detail = 1, pretty = true))
//    }

//    def test3() {
//      println("Test3")
//      val nodes = Map("1" -> node("1"),
//        "2" -> node("2"),
//        "3" -> node("3"))
//      val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
//      val decoder = new DualDecomposition(List("edgeId"), Array((":r", 1)), 1)
//      decoder.features.weights = weights(
//        List(("1", "2", ":r", 6),
//          ("2", "3", ":r", -5),
//          ("1", "3", ":r", 1)))
//      val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
//      result.graph.printTriples(detail = 1)
//    }

//    def test4() {
//      println("Test4")
//      val nodes = Map("1" -> node("1"),
//        "2" -> node("2"),
//        "3" -> node("3"),
//        "4" -> node("4"))
//      nodes("3").relations = List((":r", nodes("4")))
//      val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
//      val decoder = new DualDecomposition(List("edgeId"), Array((":r", 1)), 1)
//      //        val decoder = new Alg2(List("edgeId"), Array((":r", 1)))
//      decoder.features.weights = weights(
//        List(("1", "2", ":r", 6),
//          ("2", "3", ":r", -6), // 5
//          ("1", "3", ":r", 3))) // 1
//      val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
//      result.graph.printTriples(detail = 1)
//    }

//    def test5() {
//      println("Test5")
//      val nodes = Map("1" -> node("1"),
//        "2" -> node("2"),
//        "3" -> node("3"),
//        "4" -> node("4"))
//      nodes("3").relations = List((":r", nodes("4")))
//      val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
//      val decoder = new Alg2(List("edgeId"), Array((":r", 1)))
//      decoder.features.weights = weights(
//        List(("1", "2", ":r", 6),
//          ("2", "3", ":r", -6), // 5
//          ("1", "3", ":r", 0))) // 1
//      val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
//      result.graph.printTriples(detail = 1)
//    }

//    def samTest() {
//      println("samTest")
//      val nodes = Map("1" -> node("1"),
//        "2" -> node("2"),
//        "3" -> node("3"),
//        "4" -> node("4"))
//      val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
//      val decoder = new DualDecomposition(List("edgeId"), Array((":r", 1), (":s", 1)), 1)
//      decoder.features.weights = weights(
//        List(("1", "2", ":s", -100),
//          ("1", "3", ":r", 1),
//          ("4", "2", ":r", 1),
//          ("4", "3", ":r", -1)))
//      for { node1 <- nodes.keys
//            node2 <- nodes.keys
//            label <- List(":r", ":s")
//            if (!decoder.features.weights.fmap.contains("Id1="+node1+":Id2="+node2+":L="+label)) } {
//        decoder.features.weights.fmap("Id1="+node1+":Id2="+node2+":L="+label) = minusInfty
//      }
//      val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
//      result.graph.printTriples(detail = 1)
//    }
}
