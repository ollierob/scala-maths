package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Expression, Variable}
import net.ollie.maths.functions.numeric.Floor
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.{Integer, Natural}
import net.ollie.maths.numbers.combinatorial.BinomialCoefficient
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 18/01/14.
 */
sealed trait ChebyshevPolynomial
        extends Polynomial {

    def n: Natural

}

/**
 * @see http://mathworld.wolfram.com/ChebyshevPolynomialoftheFirstKind.html
 */
trait ChebyshevFirstKind
        extends ChebyshevPolynomial {

    def of: Expression

    override def toString = s"ChebyshevT($n)($of)"

}

object ChebyshevFirstKind {

    def apply(n: Natural)(expression: Expression): ChebyshevFirstKind = n match {
        case Zero => new TZero(expression)
        case One => new TOne(expression)
        case _ => new TAny(n, expression)
    }

}

private class TZero(val of: Expression)
extends ChebyshevFirstKind {

    def n = Zero

    def representation = One

    override def df(x: Variable) = One.df(x)

}

private class TOne(val of: Expression)
        extends ChebyshevFirstKind {

    def n = One

    def representation = of

}

private class TAny(val n: Natural, val of: Expression)
        extends ChebyshevFirstKind {

    def representation = (2 * of * ChebyshevFirstKind(n - 1)(of)) - ChebyshevFirstKind(n - 2)(of)

    override def df(x: Variable) = n * ChebyshevSecondKind(n - 1)(x)

}

/**
 * @see http://mathworld.wolfram.com/ChebyshevPolynomialoftheSecondKind.html
 */
trait ChebyshevSecondKind
        extends ChebyshevPolynomial {

    def of: Expression

    override def toString = s"ChebyshevU($n)($of)"

}

object ChebyshevSecondKind {

    def apply(n: Natural)(of: Expression): ChebyshevSecondKind = n match {
        case Zero => new UZero(of)
        case One => new UOne(of)
        case _ => new RecursiveUAny(n)(of)
    }

}

private class UZero(val of: Expression)
extends ChebyshevSecondKind {

    def n = Zero

    def representation = One

}

private class UOne(val of: Expression)
        extends ChebyshevSecondKind {

    def n = One

    def representation = 2 * of

}

private class UAny(override val n: Natural, val of: Expression)
        extends ChebyshevSecondKind {

    def representation = Series(nth, Zero, Floor(n / 2))

    private val nth = new ((Integer) => Expression) {

        def apply(r: Integer): Expression = ((-One) ^ r) * BinomialCoefficient(n - r, r) * ((2 * of) ^ (n - (2 * r)))

        override def toString = s"(-1)^r (degree-r choose r) (2x)^(degree-2r)"

    }

}

private class RecursiveUAny(override val n: Natural)(val of: Expression)
extends ChebyshevSecondKind {

    def representation = (2 * of * ChebyshevSecondKind(n - 1)(of)) - ChebyshevSecondKind(n - 2)(of)

    override def df(x: Variable) = ((n.succ) * ChebyshevFirstKind(n.succ)(of) - (of * this)) / (of ^ 2 - 1)

    override def toString = s"Chebyshev2($n)($of)"

}