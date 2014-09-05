package edu.cmu.lti.nlp.amr
import scala.collection.mutable.Map

package object ConceptInvoke {
    type OptionMap = Map[Symbol, String]

    def Decoder(options: OptionMap, oracle: Boolean = false): Decoder = {
        val stage1Features: List[String] = getStage1Features(options)
        logger(0, "Stage1 features = " + stage1Features)

        if (!options.contains('stage1ConceptTable)) {
            System.err.println("Error: No concept table specified");
            sys.exit(1)
        }
        val conceptTable: Array[PhraseConceptPair] = getConceptTable(options)
        val useNER = options.contains('ner)
        if (oracle) {
            new Oracle(stage1Features, conceptTable, useNER)
        } else {
            new Decoder1(stage1Features, conceptTable, useNER)
        }
    }

    def getConceptTable(options: OptionMap): Array[PhraseConceptPair] = {
        val conceptFile = options('stage1ConceptTable)
        Source.fromFile(conceptFile).getLines.map(x => new PhraseConceptPair(x)).toArray
    }

    def getStage1Features(options: OptionMap): List[String] = {
        options.getOrElse('stage1Features, "length,count").split(",").toList
    }
}

