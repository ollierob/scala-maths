package net.ollie.maths.numbers.complex

import net.ollie.maths.Operation.?!?
import net.ollie.maths.numbers.{Infinite, UnsignedInfinity}

/**
 * Created by Ollie on 18/01/14.
 */
object ComplexInfinity
        extends Infinite
        with ComplexNumber {

    def re = UnsignedInfinity

    def im = UnsignedInfinity

    override def abs = super[Infinite].abs

    def arg = ?!?

    override def toString = "Z∞"

}
