package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 05/01/14.
 */
object DiracDelta
        extends UnivariateFunction[Number, PositiveRealNumber] {

    def apply(n: Number): PositiveRealNumber = if (n.isEmpty) Infinity else Zero

    def apply(expression: Expression): Expression = {
        if (expression.isEmpty) Infinity
        else expression match {
            case n: Number => apply(n)
            case d: Differentiable => apply(d)
            case _ => new DiracDelta(expression)
        }
    }

    def apply(expression: Differentiable): Differentiable = {
        if (expression.isEmpty) Infinity
        else expression match {
            case n: Number => apply(n)
            case _ => new DifferentiableDiracDelta(expression)
        }
    }

    override def toString = "δ(?)"

}

class DiracDelta(val expression: Expression)
        extends Composite {

    protected[this] def of = expression

    def isEmpty = false

    protected[this] def at(n: Number) = DiracDelta(n)

    protected[this] def apply(expr: Expression) = DiracDelta(expr)

    override def toString = s"δ($of)"

}

class DifferentiableDiracDelta(val differentiable: Differentiable)
        extends DiracDelta(differentiable)
        with DifferentiableComposite {

    override def of: Differentiable = differentiable

    protected[this] def df(expression: Differentiable) = -DiracDelta(expression) / expression

}
