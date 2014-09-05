package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr.FastFeatureVector._
import edu.cmu.lti.nlp.amr._

abstract class Decoder {
    val stage1Features : ConceptInvoke.Features
    val stage2Features : GraphDecoder.Features
    var weights: FeatureVector

    def decode(i: Input) : DecoderResult
}
