package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.{Integer, Real}

/**
 * Created by Ollie on 16/02/14.
 */
object Modulo {

    def apply(dividend: Real, divisor: Real): Modulo = {
        new RealModulo(dividend, divisor)
    }

}

trait Modulo {

    def remainder: Real

    def quotient: Integer

}

class RealModulo(val dividend: Real, val divisor: Real)
        extends Modulo {

    private lazy val q: Integer = {
        Signum(divisor) * Floor(dividend / divisor.abs)
    }

    private lazy val r: Real = dividend - (divisor * q)

    def remainder = r

    def quotient = q

    override def toString = s"($dividend % $divisor)"

}
