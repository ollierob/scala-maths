package net.ollie.maths.functions.hypergeometric

import net.ollie.maths._
import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder, UnivariateFunction}
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.methods.{Integrate, SimpsonsIntegrationMethod}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.ComplexInfinity
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/GammaFunction.html
 */
trait Gamma
        extends UpperIncompleteGamma {

    final override def from = Zero

    override def toString = s"Gamma($of)"

}

object Gamma
        extends RealFunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(re: Real): Real = re match {
        case i: Integer if i.isPositive => apply(i.abs)
        case _ => new RealGamma(re)
    }

    def apply(n: Natural): Natural = (n.decr) !

    protected[this] def create(expr: Expression) = new GammaOf(expr)

    override protected[this] def empty = ComplexInfinity

}

class GammaOf(val of: Expression)
        extends BuiltFunction
        with Gamma {

    def isEmpty = false

    protected[this] def derivative(z: Expression) = Gamma(z) * Digamma(z)

    protected[this] def builder = Gamma

    override def toString = s"Gamma($of)"

}

class RealGamma(override val of: Real)
        extends Real
        with Gamma
        with CachedEvaluated {

    private def fn(t: Variable): Univariate = (t ^ (of - 1)) * Exp(-t)

    private lazy val integral = Integrate(fn _, Zero, Infinity)(SimpsonsIntegrationMethod)

    protected[this] override def doEvaluate(precision: Precision) = integral.evaluate(precision)

    override def isEmpty = false //Gamma is not zero anywhere.

    override def toConstant = super[Real].toConstant

    override def variables = super[Real].variables

}