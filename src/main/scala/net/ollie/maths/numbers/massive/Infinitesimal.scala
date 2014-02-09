package net.ollie.maths.numbers.massive

import net.ollie.maths.numbers.{Precision, Real, PositiveReal}
import net.ollie.maths.numbers.constants.Zero

/**
 * Real finite numbers that (probably) cannot be expressed in decimal form because they are so small.
 * Created by Ollie on 09/02/14.
 * @see [[Massive]]
 */
trait Infinitesimal
        extends PositiveReal {

    def isEmpty = false

    override protected def tryCompareTo(that: Real): Option[Int] = that match {
        case Zero => Some(1) //Greater than zero
        case i: Infinitesimal => Some(this compareTo i)
        case _ if that.isStrictlyPositive => Some(-1) //Smaller than positive numbers
        case _ => Some(1) //Greater than negative numbers
    }

    def compareTo(that: Infinitesimal): Int = 0

    protected[this] def doEvaluate(precision: Precision) = 0

    override def toString = "~0"

}
