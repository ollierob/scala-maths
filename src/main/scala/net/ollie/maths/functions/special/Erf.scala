package net.ollie.maths.functions.special

import net.ollie.maths._
import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.functions.numeric.{Exp, PositiveSquareRoot}
import net.ollie.maths.functions.{FunctionBuilder, OddBuiltFunction, Represented, UnivariateFunction}
import net.ollie.maths.methods.{Integrate, SimpsonsIntegrationMethod}
import net.ollie.maths.numbers.complex.{Complex, ImaginaryUnit => i}
import net.ollie.maths.numbers.constants.{Pi, Zero}
import net.ollie.maths.numbers.{Precision, Real}

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/Erf.html
 */
object Erf
        extends FunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(n: Constant): Constant = n match {
        case re: Real => apply(re)
        case z: Complex => apply(z.toReal.getOrElse(???))
        case _ => ???
    }

    def apply(re: Real) = re match {
        case Zero => empty
        case _ => new RealErf(re)
    }

    protected[this] def create(expr: Expression) = new Erf(expr)

    override protected[special] def empty = Zero

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
        extends Real
        with CachedEvaluated {

    private lazy val integral = 2 * Integrate(fn _, 0, x)(SimpsonsIntegrationMethod) / PositiveSquareRoot(Pi)

    private def fn(t: Variable): Univariate = Exp(-(t ^ 2))

    protected[this] def doEvaluate(precision: Precision) = integral.evaluate(precision)

    def isEmpty = x.isEmpty

    override def toString = s"Erf($x)"

}

/**
 * TODO
 * @see http://mathworld.wolfram.com/Erfi.html
 */
object Erfi
        extends FunctionBuilder
        with UnivariateFunction[Complex, Complex] {

    def apply(n: Constant) = n match {
        case re: Real => apply(Complex(re))
        case z: Complex => apply(z)
        case _ => ???
    }

    def apply(f: Complex): Complex = ???

    protected[this] def create(expr: Expression) = new Erfi(expr)

}

class Erfi(val of: Expression)
        extends AnyRef
        with Represented {

    private val erf = -i * Erf(i * of)

    def representation = erf

    override def toString = s"Erfi($of)"

}