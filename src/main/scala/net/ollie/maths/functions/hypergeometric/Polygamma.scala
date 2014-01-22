package net.ollie.maths.functions.hypergeometric

import net.ollie.maths._
import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.numbers.{NaturalNumber, Zero}
import net.ollie.maths.numbers.complex.ComplexInfinity

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/PolygammaFunction.html
 */
object Polygamma {

    def apply(order: NaturalNumber, n: Number): Number = ???

    def apply(order: NaturalNumber, expression: Expression) = new Polygamma(order, expression)

}

class Polygamma(val order: NaturalNumber, val of: Expression)
        extends Composite {

    protected[this] def at(n: Number) = Polygamma(order, n)

    protected[this] def apply(expr: Expression) = Polygamma(order, expr)

    def isEmpty = ???

    override def toString = s"Polygamma($order)($of)"

    protected[this] def derivative(at: Expression) = Polygamma(order.succ, at)

}

object Digamma
        extends ExpressionBuilder {

    def apply(n: Number): Number = Polygamma(Zero, n)

    protected[this] def create(expr: Expression) = Polygamma(Zero, expr)

    protected[this] def empty = ComplexInfinity

}