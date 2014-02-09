package net.ollie.maths.functions.special

import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.{FunctionBuilder, Number, Expression}

/**
 * Created by Ollie on 09/02/14.
 */
object LambertW
        extends ExpressionBuilder {

    protected[this] def empty: Expression = ???

    protected[this] def create(expr: Expression) = new LambertW(expr)

    def apply(n: Number): Number = ???

}

class LambertW(val of: Expression)
        extends FunctionBuilder {

    protected[this] def builder = LambertW

    protected[this] def derivative(x: Expression) = LambertW(x) / (x * (1 + LambertW(x)))

    def isEmpty = of.isEmpty

}
