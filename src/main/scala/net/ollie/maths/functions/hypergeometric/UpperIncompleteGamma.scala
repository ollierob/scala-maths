package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.{Univariate, Variable, Expression}
import net.ollie.maths.numbers.{Precision, Real, Infinity}
import net.ollie.maths.functions.{HomogeneousBivariateFunction, Represented}
import net.ollie.maths.methods.Integrate
import net.ollie.maths.functions.numeric.Exp

/**
 * Created by Ollie on 02/03/14.
 * @see http://mathworld.wolfram.com/IncompleteGammaFunction.html
 */
object UpperIncompleteGamma
        extends HomogeneousBivariateFunction[Real] {

    def apply(of: Real, from: Real): Real with UpperIncompleteGamma = {
        new RealUpperIncompleteGamma(of, from)
    }

    def apply(of: Expression, from: Expression): UpperIncompleteGamma = {
        new UpperIncompleteGammaOf(of, from)
    }

}

trait UpperIncompleteGamma
        extends Expression {

    def of: Expression

    def from: Expression

    override def toString = s"IncompleteGamma($from:$Infinity)($of)"

}

private class UpperIncompleteGammaOf(val a: Expression, val x: Expression)
        extends UpperIncompleteGamma
        with Represented {

    def of = a

    def from = x

    override def isEmpty = false

    override def representation = Integrate(integrand _, from, Infinity)

    private def integrand(t: Variable): Expression = {
        (t ^ (a - 1)) * Exp(-t)
    }

}

private class RealUpperIncompleteGamma(val of: Real, val from: Real)
        extends UpperIncompleteGamma
        with Real {

    private lazy val evaluator: Real = Integrate(integrand _, from, Infinity)

    private def integrand(t: Variable): Univariate = (t ^ (of - 1)) * Exp(-t)

    override def isEmpty = false

    override def evaluate(precision: Precision) = evaluator.evaluate(precision)

}