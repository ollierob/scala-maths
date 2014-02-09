package net.ollie.maths.functions

import net.ollie.maths._
import net.ollie.maths.numbers.Real

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

trait RealExpressionBuilder
        extends ExpressionBuilder {

    def apply(n: Number): Number = Real(n) match {
        case Some(re) => apply(re)
        case _ => ???
    }

    def apply(re: Real): Number

}