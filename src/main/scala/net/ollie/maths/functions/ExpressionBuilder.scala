package net.ollie.maths.functions

import net.ollie.maths._

/**
 * Created by Ollie on 11/01/14.
 */
trait ExpressionBuilder {

    def apply(n: Number): Number

    def apply(expression: Expression): Expression = expression match {
        case e if e.isEmpty => empty
        case n: Number => apply(n)
        case _ => create(expression)
    }

    protected[this] def create(expr: Expression): Expression

    protected[this] def empty: Expression

}

trait CompositeBuildable
        extends Composite {

    protected[this] def builder: ExpressionBuilder

    protected[this] def at(n: Number) = builder(n)

    protected[this] def apply(expr: Expression) = builder(expr)

}
