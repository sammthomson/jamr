package JointDecoder


import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair
import edu.cmu.lti.nlp.amr.FastFeatureVector.{Conjoined, ValuesList}
import edu.cmu.lti.nlp.amr._
import org.scalatest.Matchers

import scala.Double.{NegativeInfinity => minusInfty}
import scala.collection.{mutable => m}


/**
 * @author sthomson@cs.cmu.edu
 */
class IlpDecoderTest extends org.scalatest.FlatSpec with Matchers {
    def weights(weightList: List[(String, String, String, Double)], labelSet: Array[String]): FastFeatureVector.FeatureVector = {
        val feats = FastFeatureVector.FeatureVector(labelSet)
        for ((node1, node2, relation, weight) <- weightList) {
            feats += List(("C1=" + node1 + "+C2=" + node2, ValuesList(0.0, List(Conjoined(feats.labelToIndex(relation), weight)))))
        }
        feats
    }

    "IlpDecoder" should "get a small graph correct" in {
        val sentence = Array("a", "b", "c")
        val determinismByLabel = Array((":r", 1))
        val decoder = new JointDecoder.IlpDecoder(
            List("conceptGivenPhrase"),
            Array(
                new PhraseConceptPair("a ||| a-01 ||| c|p=10"),
                new PhraseConceptPair("b ||| b-01 ||| c|p=-1"),
                new PhraseConceptPair("c ||| c-01 ||| c|p=20")
            ),
            List("conceptBigram"),
            determinismByLabel
        )
        val labelSet = determinismByLabel.map(_._1)
        decoder.weights += FastFeatureVector.fromBasicFeatureVector(BasicFeatureVector.FeatureVector(m.Map("c|p" -> 1.0)), labelSet)
        decoder.weights += weights(
            List(
                ("a-01", "b-01", ":r", 6.0),
                ("b-01", "c-01", ":r", 5.0),
                ("a-01", "c-01", ":r", 1.0)
            ),
            labelSet
        )
        logger(1, decoder.stage2Features.weights)
        val result = decoder.decode(Input(
            None,
            sentence,
            Annotation(sentence, sentence, sentence),
            Annotation(sentence, sentence, sentence.zipWithIndex.map({ case (tok, i) => Dependency(-1, i, "nsubj")})),
            Annotation(sentence, sentence, sentence),
            Annotation(sentence, sentence, Array())
        ))
        logger(1, "Triples:")
        logger(1, result.graph.printTriples(detail = 1))
        val concepts = result.graph.nodes.map(x => x.concept).toSet
        concepts should equal(Set("a-01", "b-01", "c-01"))
        val edges = for (
            node1 <- result.graph.nodes;
            (label, node2) <- node1.relations
        ) yield {
            (node1.concept, label, node2.concept)
        }
        edges.toSet should equal(Set(
            ("a-01", ":r", "b-01"),
            ("b-01", ":r", "c-01")
        ))
    }

    it should "get a small graph with mutual requires and exclusion constraints correct" in {
        val sentence = Array("a", "b", "c")
        val determinismByLabel = Array((":r", 1))
        val decoder = new JointDecoder.IlpDecoder(
            List("conceptGivenPhrase"),
            Array(
                new PhraseConceptPair("a ||| a-01 ||| c|p=1"),
                new PhraseConceptPair("b ||| b-01 ||| c|p=1"),
                new PhraseConceptPair("c ||| c-01 ||| c|p=1"),
                new PhraseConceptPair("a b ||| (d-01 :r e-01) ||| c|p=1"),
                new PhraseConceptPair("b c ||| bc-01 ||| c|p=3")
            ),
            List("conceptBigram"),
            determinismByLabel
        )
        val labelSet = determinismByLabel.map(_._1)
        decoder.weights += FastFeatureVector.fromBasicFeatureVector(BasicFeatureVector.FeatureVector(m.Map("c|p" -> 1.0)), labelSet)
        decoder.weights += weights(
            List(
                ("d-01", "c-01", ":r", 100.0),
                ("e-01", "c-01", ":r", 3.0),
                ("c-01", "d-01", ":r", -1.0)
            ),
            labelSet
        )
        logger(1, decoder.stage2Features.weights)
        val result = decoder.decode(Input(
            None,
            sentence,
            Annotation(sentence, sentence, sentence),
            Annotation(sentence, sentence, sentence.zipWithIndex.map({ case (tok, i) => Dependency(-1, i, "nsubj")})),
            Annotation(sentence, sentence, sentence),
            Annotation(sentence, sentence, Array())
        ))
        logger(1, "Triples:")
        logger(1, result.graph.printTriples(detail = 1))
        val concepts = result.graph.nodes.map(x => x.concept).toSet
        concepts should equal(Set("c-01", "d-01", "e-01"))
        val edges = (for (
            node1 <- result.graph.nodes;
            (label, node2) <- node1.relations
        ) yield {
            (node1.concept, label, node2.concept)
        }).toSeq
        edges.size should equal(edges.toSet.size) // edges should be unique
        edges.toSet should equal(Set(
            ("d-01", ":r", "e-01"),
            ("e-01", ":r", "c-01")
        ))
    }

    it should "get a small graph with mutual requires and exclusion constraints correct2" in {
        val sentence = Array("a", "b", "c")
        val determinismByLabel = Array((":r", 1))
        val decoder = new JointDecoder.IlpDecoder(
            List("conceptGivenPhrase"),
            Array(
                new PhraseConceptPair("a ||| a-01 ||| c|p=1"),
                new PhraseConceptPair("b ||| b-01 ||| c|p=1"),
                new PhraseConceptPair("c ||| c-01 ||| c|p=1"),
                new PhraseConceptPair("a b ||| (d-01 :r e-01) ||| c|p=1"),
                new PhraseConceptPair("b c ||| bc-01 ||| c|p=3")
            ),
            List("conceptBigram"),
            determinismByLabel
        )
        val labelSet = determinismByLabel.map(_._1)
        decoder.weights += FastFeatureVector.fromBasicFeatureVector(BasicFeatureVector.FeatureVector(m.Map("c|p" -> 1.0)), labelSet)
        decoder.weights += weights(
            List(
                ("c-01", "d-01", ":r", -1.0),
                ("bc-01", "a-01", ":r", 1.0)
            ),
            labelSet
        )
        logger(1, decoder.stage2Features.weights)
        val result = decoder.decode(Input(
            None,
            sentence,
            Annotation(sentence, sentence, sentence),
            Annotation(sentence, sentence, sentence.zipWithIndex.map({ case (tok, i) => Dependency(-1, i, "nsubj")})),
            Annotation(sentence, sentence, sentence),
            Annotation(sentence, sentence, Array())
        ))
        logger(1, "Triples:")
        logger(1, result.graph.printTriples(detail = 1))
        val concepts = result.graph.nodes.map(x => x.concept).toSet
        concepts should equal(Set("a-01", "bc-01"))
        val edges = (for (
            node1 <- result.graph.nodes;
            (label, node2) <- node1.relations
        ) yield {
            (node1.concept, label, node2.concept)
        }).toSeq
        edges.size should equal(edges.toSet.size) // edges should be unique
        edges.toSet should equal(Set(
            ("bc-01", ":r", "a-01")
        ))
    }
}
