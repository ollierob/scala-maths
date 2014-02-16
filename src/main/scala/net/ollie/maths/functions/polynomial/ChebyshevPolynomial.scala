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
        extends ChebyshevPolynomial

object ChebyshevFirstKind {

    def apply(n: Natural)(expression: Expression): ChebyshevFirstKind = n match {
        case Zero => TZero
        case One => new TOne(expression)
        case _ => new TAny(n, expression)
    }

}

private object TZero
        extends ChebyshevFirstKind {

    def n = Zero

    def f = One

    override def df(x: Variable) = One.df(x)

    override def toString = "Chebyshev1(0)()"

}

private class TOne(val of: Expression)
        extends ChebyshevFirstKind {

    def n = One

    def f = of

    override def toString = s"Chebyshev1(1)($of)"

}

private class TAny(val n: Natural, val of: Expression)
        extends ChebyshevFirstKind {

    def f = (2 * of * ChebyshevFirstKind(n - 1)(of)) - ChebyshevFirstKind(n - 2)(of)

    override def df(x: Variable) = n * ChebyshevSecondKind(n - 1)(x)

    override def toString = s"Chebyshev1($n)($of)"

}

/**
 * @see http://mathworld.wolfram.com/ChebyshevPolynomialoftheSecondKind.html
 */
trait ChebyshevSecondKind
        extends ChebyshevPolynomial

object ChebyshevSecondKind {

    def apply(n: Natural)(diff: Expression): ChebyshevSecondKind = n match {
        case Zero => UZero
        case One => new UOne(diff)
        case _ => new RecursiveUAny(n, diff)
    }

}

private object UZero
        extends ChebyshevSecondKind {

    def n = Zero

    def f = One

    override def toString = "Chebyshev2(0)()"

}

private class UOne(val of: Expression)
        extends ChebyshevSecondKind {

    def n = One

    def f = 2 * of

    override def toString = s"Chebyshev2(1)($of)"

}

private class UAny(override val n: Natural, val of: Expression)
        extends ChebyshevSecondKind {

    def f = Series(nth, Zero, Floor(n / 2))

    private val nth = new ((Integer) => Expression) {

        def apply(r: Integer): Expression = ((-One) ^ r) * BinomialCoefficient(n - r, r) * ((2 * of) ^ (n - (2 * r)))

        override def toString = s"(-1)^r (degree-r choose r) (2x)^(degree-2r)"

    }

}

private class RecursiveUAny(override val n: Natural, val of: Expression)
        extends ChebyshevSecondKind {

    def f = (2 * of * ChebyshevSecondKind(n - 1)(of)) - ChebyshevSecondKind(n - 2)(of)

    override def df(x: Variable) = ((n.succ) * ChebyshevFirstKind(n.succ)(of) - (of * this)) / (of ^ 2 - 1)

    override def toString = s"Chebyshev2($n)($of)"

}