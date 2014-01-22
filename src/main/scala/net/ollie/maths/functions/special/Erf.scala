package net.ollie.maths.functions.special

import net.ollie.maths.{Expression, Number}
import net.ollie.maths.functions.{CompositeBuilder, ExpressionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.functions.numeric.{Exp, PositiveSquareRoot}
import net.ollie.maths.methods.{Integral, SimpsonsIntegrationMethod}
import net.ollie.maths.numbers.{Precision, RealNumber, Zero}
import net.ollie.maths.numbers.complex.{ComplexNumber, ImaginaryUnit => i}
import net.ollie.maths.numbers.real.Pi

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/Erf.html
 */
object Erf
        extends ExpressionBuilder
        with UnivariateFunction[RealNumber, RealNumber] {

    def apply(n: Number): Number = n match {
        case re: RealNumber => apply(re)
        case z: ComplexNumber => apply(z.toReal.getOrElse(???))
        case _ => ???
    }

    def apply(re: RealNumber) = re match {
        case Zero => empty
        case _ => new RealErf(re)
    }

    protected[this] def create(expr: Expression) = new Erf(expr)

    protected[special] def empty = Zero

}

class Erf(val of: Expression)
        extends AnyRef
        with CompositeBuilder {

    protected[this] def builder = Erf

    protected[this] def derivative(z: Expression) = 2 * Exp(-(z ^ 2)) / PositiveSquareRoot(Pi)

    def isEmpty = of.isEmpty

    override def toString = s"Erf($of)"

}

class RealErf(val x: RealNumber)
        extends RealNumber {

    private lazy val integral = 2 * Integral(t => Exp(-(t ^ 2)), 0, x)(SimpsonsIntegrationMethod) / PositiveSquareRoot(Pi)

    protected[this] def eval(precision: Precision) = integral.evaluate(precision)

    def isEmpty = x.isEmpty

    override def toString = s"Erf($x)"

}

/**
 * TODO
 * @see http://mathworld.wolfram.com/Erfi.html
 */
object Erfi
        extends ExpressionBuilder
        with UnivariateFunction[ComplexNumber, ComplexNumber] {

    def apply(n: Number) = n match {
        case re: RealNumber => apply(ComplexNumber(re))
        case z: ComplexNumber => apply(z)
        case _ => ???
    }

    def apply(f: ComplexNumber): ComplexNumber = ???

    protected[this] def create(expr: Expression) = new Erfi(expr)

    protected[this] def empty = Zero

}

class Erfi(val of: Expression)
        extends AnyRef
        with Represented {

    private val erf = -i * Erf(i * of)

    protected[this] def f = erf

    override def toString = s"Erfi($of)"

}