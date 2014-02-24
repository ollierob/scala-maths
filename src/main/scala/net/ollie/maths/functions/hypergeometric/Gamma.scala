package net.ollie.maths.functions.hypergeometric

import net.ollie.maths._
import net.ollie.maths.functions.{RealFunctionBuilder, UnivariateFunction}
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.methods.{Integrate, SimpsonsIntegrationMethod}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.ComplexInfinity
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/GammaFunction.html
 */
object Gamma
        extends RealFunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(re: Real): Real = re match {
        case i: Integer if i.isStrictlyPositive => apply(i.abs)
        case _ => new RealGamma(re)
    }

    def apply(n: Natural): Natural = (n.decr) !

    protected[this] def create(expr: Expression) = new GammaOf(expr)

    protected[this] def empty = ComplexInfinity

}

trait Gamma {

    def of: Expression

    override def toString = s"Gamma($of)"

}

class GammaOf(val of: Expression)
        extends Function
        with Gamma {

    protected[this] def at(n: Number) = Gamma(n)

    protected[this] def apply(expr: Expression) = Gamma(expr)

    def isEmpty = false

    protected[this] def derivative(z: Expression) = Gamma(z) * Digamma(z)

    override def toString = s"Gamma($of)"

}

class RealGamma(override val of: Real)
        extends Real
        with Gamma
        with CachedEvaluated {

    private lazy val integral = Integrate(fn _, Zero, Infinity)(SimpsonsIntegrationMethod)

    private def fn(t: Variable): Univariate = (t ^ (of - 1)) * Exp(-t)

    protected[this] override def doEvaluate(precision: Precision) = integral.evaluate(precision)

    override def isEmpty = false //Gamma is not zero anywhere.

    override def toConstant = super[Real].toConstant

    override def variables = super[Real].variables

}