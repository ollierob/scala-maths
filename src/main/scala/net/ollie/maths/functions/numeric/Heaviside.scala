package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{BuiltFunction, FunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.{Half, One, Zero}

/**
 * Created by Ollie on 10/01/14.
 */
object Heaviside
        extends UnivariateFunction[Real, Real]
        with FunctionBuilder {

    def apply(n: Constant): Constant = n match {
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(f: Real): Real = f match {
        case Zero => empty
        case _ if f.isPositive => One
        case _ => Zero
    }

    protected[this] def create(expr: Expression) = new Heaviside(expr)

    override def toString = "Heaviside(?)"

    protected[this] def empty = Half

}

class Heaviside(val of: Expression)
        extends AnyRef
        with BuiltFunction {

    protected[this] def builder = Heaviside

    protected[this] def derivative(x: Expression) = DiracDelta(x)

    def isEmpty = false

}
