/**
 * Add a weight to anything!
 *
 * @author sthomson@cs.cmu.edu
 */
package edu.cmu.lti.nlp.amr.util

trait HasWeight {
  def weight: Double
}

object HasWeight {
  implicit def orderByWeight[T <: HasWeight]: Ordering[T] = Ordering.by((_: T).weight)
}

case class Weighted[T](value: T, weight: Double) extends HasWeight {
  override def toString: String = s"$value % $weight"
}

object Weighted {
  implicit def unwrap[T](x: Weighted[T]): T = x.value

  // sugar
  val % = Weighted  // for unapply / pattern matching

  case class WeightedAssoc[T](private val _value: T) {
    def % (weight: Double): Weighted[T] = Weighted(_value, weight)
  }

  implicit def any2WeightedAssoc[T](value: T): WeightedAssoc[T] = WeightedAssoc(value)
}
