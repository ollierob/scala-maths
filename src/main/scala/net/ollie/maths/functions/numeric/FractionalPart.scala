package net.ollie.maths.functions.numeric

import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers.{Rational, Precision, Integer, Real}
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 26/02/14.
 */
object FractionalPart
        extends UnivariateFunction[Real, Real] {

    def apply(i: Integer): Real = Zero

    def apply(x: Real): Real = x match {
        case i: Integer => apply(i)
        case _ => new FractionalPart(x)
    }

    private class FractionalPart(val x: Real)
            extends Real {

        private lazy val eval = Signum(x) * (x.abs - Floor(x.abs))

        def isEmpty = eval.isEmpty

        def evaluate(precision: Precision) = eval.evaluate(precision)

        override def toString = s"FractionalPart($x)"

    }

}

object IntegerPart
        extends UnivariateFunction[Real, Integer] {

    def apply(i: Integer): Integer = i

    def apply(x: Real): Integer = x match {
        case i: Integer => apply(i)
        case r: Rational => Modulo(r.numerator, r.denominator).quotient
        case _ => new IntegerPart(x)
    }

    private class IntegerPart(val x: Real)
            extends Integer {

        private lazy val eval = Signum(x) * Floor(x.abs)

        def evaluate: BigInt = eval.evaluate

        override def toString = s"IntegerPart($x)"

    }

}