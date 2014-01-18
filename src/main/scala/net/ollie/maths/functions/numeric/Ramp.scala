package net.ollie.maths.functions.numeric

import net.ollie.maths.{Differentiable, DifferentiableRepresented, Expression, Number}
import net.ollie.maths.functions.{DifferentiableExpressionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.numbers.{RealNumber, Zero}

/**
 * Created by Ollie on 11/01/14.
 */
object Ramp
        extends UnivariateFunction[RealNumber, RealNumber]
        with DifferentiableExpressionBuilder {

    def apply(n: Number): Number = n match {
        case re: RealNumber => apply(re)
        case _ => ???
    }

    def apply(re: RealNumber) = re match {
        case _ if re >= 0 => re
        case _ => 0
    }

    protected[this] def create(expr: Expression) = new Ramp(expr)

    protected[this] def create(diff: Differentiable) = new DifferentiableRamp(diff)

    protected[this] def empty = Zero

    override def toString = "Ramp(?)"

}

class Ramp(val expression: Expression)
        extends Represented {

    protected[this] def f = expression * Heaviside(expression)

    override def toString = s"Ramp($expression)"

}

class DifferentiableRamp(override val expression: Differentiable)
        extends Ramp(expression)
        with DifferentiableRepresented {

    override def f = expression * Heaviside(expression)

}
