package net.ollie.maths.numbers

import net.ollie.maths.numbers.Precision

/**
 * Created by Ollie on 09/02/14.
 */
trait Infinitesimal
        extends PositiveReal {

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
