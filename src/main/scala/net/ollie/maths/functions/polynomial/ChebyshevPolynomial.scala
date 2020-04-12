package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.numeric.Floor
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.{Integer, Natural}
import net.ollie.maths.numbers.combinatorial.BinomialCoefficient
import net.ollie.maths.numbers.constants.{One, Zero}

/**
 * Created by Ollie on 18/01/14.
 */
sealed trait ChebyshevPolynomial
        extends Polynomial


/**
 * @see http://mathworld.wolfram.com/ChebyshevPolynomialoftheFirstKind.html
 */
trait ChebyshevFirstKind
        extends ChebyshevPolynomial {

    def of: Expression

    override def toString = s"ChebyshevFirst($degree)($of)"

}

object ChebyshevFirstKind {

    def apply(n: Natural)(expression: Expression): ChebyshevFirstKind = n match {
        case Zero => new TZero(expression)
        case One => new TOne(expression)
        case _ => new TAny(n)(expression)
    }

}

private class TZero(val of: Expression)
        extends ChebyshevFirstKind {

    def degree = Zero

    def representation = One

    override def df(x: Variable) = One.df(x)

}

private class TOne(val of: Expression)
        extends ChebyshevFirstKind {

    def degree = One

    def representation = of

}

private class TAny(val degree: Natural)(val of: Expression)
        extends ChebyshevFirstKind {

    def representation = (2 * of * ChebyshevFirstKind(degree - 1)(of)) - ChebyshevFirstKind(degree - 2)(of)

    override def df(x: Variable) = degree * ChebyshevSecondKind(degree - 1)(x)

}

/**
 * @see http://mathworld.wolfram.com/ChebyshevPolynomialoftheSecondKind.html
 */
trait ChebyshevSecondKind
        extends ChebyshevPolynomial {

    def of: Expression

    override def toString = s"ChebyshevSecond($degree)($of)"

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

    override def degree = Zero

    def representation = One

}

private class UOne(val of: Expression)
        extends ChebyshevSecondKind {

    def degree = One

    def representation = 2 * of

}

private class UAny(override val degree: Natural, val of: Expression)
        extends ChebyshevSecondKind {

    def representation = Series(nth _, Zero, Floor(degree / 2))

    private def nth(r: Integer): Expression = {
        ((-One) ^ r) * BinomialCoefficient(degree - r, r) * ((2 * of) ^ (degree - (2 * r)))

    }

}

private class RecursiveUAny(override val degree: Natural)(val of: Expression)
        extends ChebyshevSecondKind {

    def representation = (2 * of * ChebyshevSecondKind(degree - 1)(of)) - ChebyshevSecondKind(degree - 2)(of)

    override def df(x: Variable) = ((degree.succ) * ChebyshevFirstKind(degree.succ)(of) - (of * this)) / (of ^ 2 - 1)

}