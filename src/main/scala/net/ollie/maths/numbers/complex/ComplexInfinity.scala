package net.ollie.maths.numbers.complex

import net.ollie.maths.Operation.indeterminate
import net.ollie.maths.numbers.{Infinite, UnsignedInfinity}

/**
 * Created by Ollie on 18/01/14.
 */
object ComplexInfinity
        extends Infinite
        with ComplexNumber {

    def re = UnsignedInfinity

    def im = UnsignedInfinity

    override def unary_-() = this

    override def abs = super[Infinite].abs

    override def arg = indeterminate

    override def isEmpty = super[Infinite].isEmpty

    override def toString = "Z∞"

}
