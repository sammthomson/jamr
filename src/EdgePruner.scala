package edu.cmu.lti.nlp.amr

/**
 * A rule-based pruner so we don't have to consider every label for every edge.
 * Rules were found by grouping all edges by either src, dest, or relation label,
 * and finding groups that had > 10 items and were >= 99% deterministic.
 *
 * The EdgePruner prunes 37 (of 69289) gold edges in the training data, which is 99.95% recall,
 * and 41 (of 34728) gold edges in the dev data, which is 99.88% recall.
 *
 * @author sthomson@cs.cmu.edu
 */
class EdgePruner() {
    import edu.cmu.lti.nlp.amr.EdgePruner._

    def validEdge(src: Node, dest: Node, relation: String): Boolean = {
        if (src.isConstant) {
            false
        } else if (src.concept == "name") {
            relation == ":op"
        } else if (destDeterminedByRelation.contains(relation) &&
                    dest.concept != destDeterminedByRelation(relation)) {
            false
        } else if (relationDeterminedByDest.contains(dest.concept) &&
                    relation != relationDeterminedByDest(dest.concept)) {
            false
        } else if (takesOnlyConsts.contains(relation) && !dest.isConstant) {
            false
        } else if (dest.isConstant &&
                    !takesConstsOrNot.contains(relation) &&
                    !takesOnlyConsts.contains(relation) &&
                    !srcTakesConst.contains(src.concept)) {
            false
        } else if (srcCantBePropBank.contains(relation) && src.isPropBank) {
            false
        } else if (srcMustBePropBank.contains(relation) && !src.isPropBank) {
            false
        } else if (relation.startsWith(":op") && !takesOps.contains(src.concept)) {
            false
        } else if (src.concept == "multi-sentence") {
            relation.startsWith(":snt")
        } else if (onlyDateEntity.contains(relation)) {
            src.concept == "date-entity"
        } else if (src.concept == "date-entity") {
            possiblyDateEntity.contains(relation)
        } else {
            true
        }
    }
}
object EdgePruner {
    val onlyDateEntity = Set(
        ":calendar",
        ":century",
        ":day",
        ":dayperiod",
        ":decade",
        ":month",
        ":quarter",
        ":season",
        ":timezone",
        ":weekday",
        ":year"
    )
    val possiblyDateEntity = Set(
        ":domain",
        ":mod",
        ":part",
        ":quant",
        ":time",
        ":unit"
    )
    val destDeterminedByRelation = Map(
        ":name" -> "name",
        ":ord" -> "ordinal-entity",
        ":polarity" -> "-"
    )
    val relationDeterminedByDest = Map(
        "-" -> ":polarity",
        "also" -> ":mod"  // 110 / 111
    )
    val takesOnlyConsts = Set(
        ":century",
        ":day",
        ":decade",
        ":li",
        ":month",
        ":polarity",
        ":quarter",
        ":timezone",
        ":value",
        ":year"
    )
    val takesConstsOrNot = Set(
        ":frequency",
        ":op",
        ":quant",
        ":time",
        ":value"
    )
    val srcTakesConst = Set(
        "estimate-01",
        "exceed-01",
        "have-quant-91",
        "include-91",
        "number-01"
    )
    val srcMustBePropBank = Set(
        ":ARG0",
        ":ARG2",
        ":ARG3",
        ":ARG4",
        ":ARG5",
        ":cause",
        ":li",
        ":prep-at",
        ":prep-in-addition-to",
        ":prep-into",
        ":prep-toward",
        ":prep-within"
    )
    val srcCantBePropBank = Set(
        ":op",
        ":poss",
        ":prep-among",
        ":prep-between",
        ":prep-per",
        ":range",
        ":scale",
        ":snt1",
        ":snt2",
        ":value"
    )

    def takesOps = Set(
        "about",
        "above",
        "across",
        "after",
        "against",
        "ago",
        "ahead",
        "all",
        "almost",
        "along",
        "alongside",
        "among",
        "and",
        "any",
        "approximate",
        "approximately",
        "around",
        "as-early-as",
        "as-far-as",
        "as-low-as",
        "as-many-as",
        "as-much-as",
        "as-of",
        "at-least",
        "away",
        "before",
        "behind",
        "beside",
        "between",
        "beyond",
        "border",
        "both",
        "by",
        "category",
        "close",
        "close-to",
        "contrast",
        "country",
        "date-interval",
        "day",
        "duration",
        "during",
        "early",
        "east",
        "end",
        "even-if",
        "few",
        "follow",
        "following",
        "from",
        "inside",
        "into",
        "land",
        "late",
        "less-than",
        "likely",
        "location",
        "mid",
        "middle",
        "minimum",
        "more",
        "more-than",
        "multiple",
        "name",
        "near",
        "nearly",
        "next",
        "next-to",
        "no-more-than",
        "northwest",
        "off",
        "once",
        "only",
        "or",
        "out",
        "out-of",
        "outside",
        "over",
        "pan",
        "part-of",
        "past",
        "period",
        "possible",
        "post",
        "prior",
        "product-of",
        "recent",
        "regard",
        "relative-position",
        "role",
        "series",
        "several",
        "since",
        "slash",
        "some",
        "soon",
        "south",
        "southeast",
        "southernmost",
        "start",
        "then",
        "through",
        "throughout",
        "to",
        "under",
        "until",
        "up-to",
        "wake",
        "warrant",
        "within",
        "year"
    )
}
