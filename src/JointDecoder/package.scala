package edu.cmu.lti.nlp.amr

import scala.collection.mutable.Map

package object JointDecoder {
    def getFeatures(options: Map[Symbol, String]) : List[String] = {
        options.getOrElse('stage2Features, "conceptBigram,rootConcept").split(",").toList.filter(x => x != "edgeId" && x != "labelWithId")
    }

    def Decoder(options: Map[Symbol, String], oracle: Boolean = false): JointDecoder.Decoder = {
        if (!options.contains('stage1ConceptTable)) throw new IllegalArgumentException("Error: No concept table specified")
        if (!options.contains('stage2Labelset)) throw new IllegalArgumentException("Error: No labelset file specified")
        val stage1Features = ConceptInvoke.getStage1Features(options)
        val conceptTable = ConceptInvoke.getConceptTable(options)
        val stage2Labelset = GraphDecoder.getLabelset(options)
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

    def Oracle(options: Map[Symbol, String]): JointDecoder.Decoder = {
        return new Oracle(
            ConceptInvoke.getStage1Features(options),
            ConceptInvoke.getConceptTable(options),
            GraphDecoder.getFeatures(options),
            GraphDecoder.getLabelset(options)
        )
    }
}
