package net.ollie.maths.functions.hypergeometric

import net.ollie.maths._
import net.ollie.maths.functions.{ExpressionBuilder, UnivariateFunction}
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.methods.{Integral, SimpsonsIntegrationMethod}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{ComplexInfinity, Complex}

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/GammaFunction.html
 */
object Gamma
        extends ExpressionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(re: Real) = re match {
        case n: Natural => apply(n)
        case _ => new RealGamma(re)
    }

    def apply(n: Natural) = (n.decr) !

    protected[this] def create(expr: Expression) = new Gamma(expr)

    protected[this] def empty = ComplexInfinity

}

class Gamma(val of: Expression)
        extends Composite {

    protected[this] def at(n: Number) = Gamma(n)

    protected[this] def apply(expr: Expression) = Gamma(expr)

    def isEmpty = false

    protected[this] def derivative(z: Expression) = Gamma(z) * Digamma(z)

    override def toString = s"Gamma($of)"

}

class RealGamma(val z: Complex)
        extends Gamma(z)
        with Real {

    private lazy val integral = Integral(t => (t ^ (z - 1)) * Exp(-t), Zero, Infinity)(SimpsonsIntegrationMethod)

    protected[this] def eval(precision: Precision) = integral.evaluate(precision)

    override def isEmpty = false //Gamma is not zero anywhere.

    override def toConstant = super[Real].toConstant

    override def variables = super[Real].variables

}