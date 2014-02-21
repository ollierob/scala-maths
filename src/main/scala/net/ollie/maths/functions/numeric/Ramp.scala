package net.ollie.maths.functions.numeric

import net.ollie.maths.{Expression, Number}
import net.ollie.maths.functions.{FunctionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 11/01/14.
 */
object Ramp
        extends UnivariateFunction[Real, Real]
        with FunctionBuilder {

    def apply(n: Number): Number = n match {
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(re: Real) = re match {
        case _ if re >= 0 => re
        case _ => 0
    }

    protected[this] def create(expr: Expression) = new RampOf(expr)

    protected[this] def empty = Zero

    override def toString = "Ramp(?)"

}

class RampOf(val expression: Expression)
        extends Represented {

    def representation = expression * Heaviside(expression)

    override def toString = s"Ramp($expression)"

}
