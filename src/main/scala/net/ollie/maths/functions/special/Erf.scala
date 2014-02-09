package net.ollie.maths.functions.special

import net.ollie.maths.{OddBuiltFunction, Expression, Number}
import net.ollie.maths.functions.{ExpressionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.functions.numeric.{Exp, PositiveSquareRoot}
import net.ollie.maths.methods.{Integral, SimpsonsIntegrationMethod}
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.numbers.complex.{Complex, ImaginaryUnit => i}
import net.ollie.maths.numbers.constants.{Zero, Pi}

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/Erf.html
 */
object Erf
        extends ExpressionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(n: Number): Number = n match {
        case re: Real => apply(re)
        case z: Complex => apply(z.toReal.getOrElse(???))
        case _ => ???
    }

    def apply(re: Real) = re match {
        case Zero => empty
        case _ => new RealErf(re)
    }

    protected[this] def create(expr: Expression) = new Erf(expr)

    protected[special] def empty = Zero

}

class Erf(val of: Expression)
        extends AnyRef
        with OddBuiltFunction {

    protected[this] def builder = Erf

    protected[this] def derivative(z: Expression) = 2 * Exp(-(z ^ 2)) / PositiveSquareRoot(Pi)

    def isEmpty = of.isEmpty

    override def toString = s"Erf($of)"

}

class RealErf(val x: Real)
        extends Real {

    private lazy val integral = 2 * Integral(t => Exp(-(t ^ 2)), 0, x)(SimpsonsIntegrationMethod) / PositiveSquareRoot(Pi)

    protected[this] def doEvaluate(precision: Precision) = integral.evaluate(precision)

    def isEmpty = x.isEmpty

    override def toString = s"Erf($x)"

}

/**
 * TODO
 * @see http://mathworld.wolfram.com/Erfi.html
 */
object Erfi
        extends ExpressionBuilder
        with UnivariateFunction[Complex, Complex] {

    def apply(n: Number) = n match {
        case re: Real => apply(Complex(re))
        case z: Complex => apply(z)
        case _ => ???
    }

    def apply(f: Complex): Complex = ???

    protected[this] def create(expr: Expression) = new Erfi(expr)

    protected[this] def empty = Zero

}

class Erfi(val of: Expression)
        extends AnyRef
        with Represented {

    private val erf = -i * Erf(i * of)

    def f = erf

    override def toString = s"Erfi($of)"

}