package net.ollie.maths.functions

import net.ollie.maths._
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.complex.Complex

/**
 * Can convert an expression into another expression, or a number into another number.
 *
 * Created by Ollie on 11/01/14.
 * @see [[BuiltFunction]]
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
        case _ => otherwise(n)
    }

    protected def otherwise(n: Number): Number = ???

    def apply(re: Real): Number

}

trait ComplexExpressionBuilder
        extends RealExpressionBuilder {

    protected override def otherwise(n: Number): Number = Complex(n) match {
        case Some(z) => apply(z)
        case _ => super.apply(n)
    }

    def apply(z: Complex): Number

}