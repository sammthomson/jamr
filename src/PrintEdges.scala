/*
e.g.:

    ./run PrintEdges "{JAMR_HOME}/data/LDC-2013-Sep/amr-release-proxy.train.aligned.no_opN"
*/
package edu.cmu.lti.nlp.amr

import edu.cmu.lti.nlp.amr.Corpus._


object Edges {
    def getAllEdges(filename: String): Iterator[(Node, String, Node)] = {
        val trainLines: Iterator[String] = Source.fromFile(filename).getLines()
        val data = getAmrBlocks(trainLines).map(x =>
            AMRTrainingData(x).toOracleGraph(clearUnalignedNodes = false)
        )
        for (
            g <- data;
            src <- g.nodes;
            (relation, dest) <- src.relations
        ) yield {
            (src, relation, dest)
        }
    }
}

object PrintEdges extends App {
    val trainFilename: String = args(0)

    val allEdges = Edges.getAllEdges(trainFilename)

    println(Seq(
        "src.concept",
        "src.takesArgs",
        "src.takesOps",
        "src.isConstant",
        "relation",
        "dest.concept",
        "dest.takesArgs",
        "dest.takesOps",
        "dest.isConstant"
    ).mkString("\t"))
    for ((src, relation, dest) <- allEdges) {
        println(Seq(
            src.concept,
            src.takesArgs,
            src.takesOps,
            src.isConstant,
            relation,
            dest.concept,
            dest.takesArgs,
            dest.takesOps,
            dest.isConstant
        ).mkString("\t"))
    }
}
