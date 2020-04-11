package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{BuiltFunction, FunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 05/01/14.
 */
object DiracDelta
        extends UnivariateFunction[Constant, PositiveReal]
        with FunctionBuilder {

    def apply(n: Constant): PositiveReal = {
        if (n.isEmpty) Infinity
        else Zero
    }

    protected[this] def create(expr: Expression) = new DiracDeltaOf(expr)

    protected[this] def empty = Infinity

    override def toString = "δ(?)"

}

class DiracDeltaOf(val expression: Expression)
        extends BuiltFunction {

    protected[this] def of = expression

    def isEmpty = !expression.isEmpty

    protected[this] def derivative(at: Expression) = -DiracDelta(at) / at

    override protected[this] def builder = DiracDelta

    override def toString = s"δ($of)"

}
