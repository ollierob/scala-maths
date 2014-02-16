package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.{Precision, Real}

/**
 * Created by Ollie on 16/02/14.
 */
object Modulo {

    def apply(dividend: Real, divisor: Real): Real = {
        new Modulo(dividend, divisor)
    }

}

class Modulo(val dividend: Real, val divisor: Real)
        extends Real {

    private lazy val q: Real = Signum(divisor) * Floor(dividend / divisor.abs)

    private lazy val r: Real = dividend - (divisor * q)

    def remainder: Real = r

    def quotient: Real = q

    def isEmpty = r.isEmpty

    protected[this] def doEvaluate(precision: Precision) = r.evaluate(precision)

    override def toString = s"($dividend % $divisor)"

}
