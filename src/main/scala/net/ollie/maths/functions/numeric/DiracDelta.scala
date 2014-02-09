package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.{ExpressionBuilder, UnivariateFunction}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 05/01/14.
 */
object DiracDelta
        extends UnivariateFunction[Number, PositiveReal]
        with ExpressionBuilder {

    def apply(n: Number): PositiveReal = if (n.isEmpty) Infinity else Zero

    protected[this] def create(expr: Expression) = new DiracDelta(expr)

    protected[this] def empty = Infinity

    override def toString = "δ(?)"

}

class DiracDelta(val expression: Expression)
        extends Function {

    protected[this] def of = expression

    def isEmpty = false

    protected[this] def at(n: Number) = DiracDelta(n)

    protected[this] def apply(expr: Expression) = DiracDelta(expr)

    protected[this] def derivative(at: Expression) = -DiracDelta(at) / at

    override def toString = s"δ($of)"

}
