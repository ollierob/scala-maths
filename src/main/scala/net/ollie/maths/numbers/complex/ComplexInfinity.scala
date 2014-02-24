package net.ollie.maths.numbers.complex

import net.ollie.maths.Operation.indeterminate
import net.ollie.maths.numbers.{Precision, Infinite, UnsignedInfinity}

/**
 * Created by Ollie on 18/01/14.
 */
object ComplexInfinity
        extends Infinite
        with Complex {

    def re = UnsignedInfinity

    def im = UnsignedInfinity

    override def unary_-() = this

    override def abs = super[Infinite].abs

    override def arg = indeterminate

    override def isEmpty = super[Infinite].isEmpty

    override def tryEvaluate(precision: Precision) = super[Infinite].tryEvaluate(precision)

    override def toString = "Z∞"

}
