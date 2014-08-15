package edu.cmu.lti.nlp.amr

import scala.collection.mutable.Map

package object JointDecoder {
  def Decoder(options: Map[Symbol, String], oracle: Boolean = false) : JointDecoder.Decoder = {
    if (!options.contains('stage1ConceptTable)) throw new IllegalArgumentException("Error: No concept table specified")
    if (!options.contains('stage2Labelset)) throw new IllegalArgumentException("Error: No labelset file specified")
    val stage1Features = options.getOrElse('stage1Features,"length,count").split(",").toList
    val conceptTable = Source.fromFile(options('stage1ConceptTable)).getLines().map(new ConceptInvoke.PhraseConceptPair(_)).toArray
    val stage2Labelset: Array[(String, Int)] = GraphDecoder.getLabelset(options)
    val stage2Features = GraphDecoder.getFeatures(options)
    if (oracle) {
      new Oracle(stage1Features, conceptTable, stage2Features, stage2Labelset)
    } else {
      options('jointDecoder) match {
        case "ILP" => new IlpDecoder(stage1Features, conceptTable, stage2Features, stage2Labelset)
        case x => throw new IllegalArgumentException("Error: unknown joint decoder " + x)
      }
    }
  }
}
