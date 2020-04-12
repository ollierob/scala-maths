package net.ollie.maths.functions.numeric

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{RealFunctionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 11/01/14.
 */
object Ramp
        extends RealFunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(re: Real): Real = re match {
        case _ if re.isPositive => re
        case Zero => Zero
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
