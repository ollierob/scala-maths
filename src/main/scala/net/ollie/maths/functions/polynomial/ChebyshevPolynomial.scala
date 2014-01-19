package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Expression, Variable}
import net.ollie.maths.functions.numeric.Floor
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber, One, Zero}
import net.ollie.maths.numbers.combinatorial.BinomialCoefficient

/**
 * Created by Ollie on 18/01/14.
 */
sealed trait ChebyshevPolynomial
        extends Polynomial {

    def n: NaturalNumber

}

/**
 * @see http://mathworld.wolfram.com/ChebyshevPolynomialoftheFirstKind.html
 */
trait ChebyshevFirstKind
        extends ChebyshevPolynomial

object ChebyshevFirstKind {

    def apply(n: NaturalNumber)(expression: Expression): ChebyshevFirstKind = n match {
        case Zero => TZero
        case One => new TOne(expression)
        case _ => new TAny(n, expression)
    }

}

private object TZero
        extends ChebyshevFirstKind {

    def n = Zero

    override protected[this] def f = One

    override def df(x: Variable) = One.df(x)

    override def toString = "Chebyshev1(0)()"

}

private class TOne(val of: Expression)
        extends ChebyshevFirstKind {

    def n = One

    protected[this] def f = of

    override def toString = s"Chebyshev1(1)($of)"

}

private class TAny(val n: NaturalNumber, val of: Expression)
        extends ChebyshevFirstKind {

    protected[this] def f = (2 * of * ChebyshevFirstKind(n - 1)(of)) - ChebyshevFirstKind(n - 2)(of)

    override def df(x: Variable) = (n - 1) * ChebyshevSecondKind(n - 1)(x)

    override def toString = s"Chebyshev1($n)($of)"

}

/**
 * @see http://mathworld.wolfram.com/ChebyshevPolynomialoftheSecondKind.html
 */
trait ChebyshevSecondKind
        extends ChebyshevPolynomial

object ChebyshevSecondKind {

    def apply(n: NaturalNumber)(diff: Expression): ChebyshevSecondKind = n match {
        case Zero => UZero
        case One => new UOne(diff)
        case _ => new RecursiveUAny(n, diff)
    }

}

private object UZero
        extends ChebyshevSecondKind {

    def n = Zero

    protected[this] def f = One

    override def toString = "Chebyshev2(0)()"

}

private class UOne(val of: Expression)
        extends ChebyshevSecondKind {

    def n = One

    protected[this] def f = 2 * of

    override def toString = s"Chebyshev2(1)($of)"

}

private class UAny(override val n: NaturalNumber, val of: Expression)
        extends ChebyshevSecondKind {

    protected[this] def f = Series(nth, Zero, Floor(n / 2))

    private val nth = new ((IntegerNumber) => Expression) {

        def apply(r: IntegerNumber) = ((-One) ^ r) * BinomialCoefficient(n - r, r) * ((2 * of) ^ (n - (2 * r)))

        override def toString = s"(-1)^r (n-r choose r) (2x)^(n-2r)"

    }

}

private class RecursiveUAny(override val n: NaturalNumber, val of: Expression)
        extends ChebyshevSecondKind {

    protected[this] def f = (2 * of * ChebyshevSecondKind(n - 1)(of)) - ChebyshevSecondKind(n - 2)(of)

    override def df(x: Variable) = ((n.succ) * ChebyshevFirstKind(n.succ)(of) - (of * this)) / (of ^ 2 - 1)

    override def toString = s"Chebyshev2($n)($of)"

}