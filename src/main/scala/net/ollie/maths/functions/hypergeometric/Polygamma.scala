package net.ollie.maths.functions.hypergeometric

import net.ollie.maths._
import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.complex.ComplexInfinity
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/PolygammaFunction.html
 */
object Polygamma {

    def apply(order: Natural, n: Number): Number = ???

    def apply(order: Natural, expression: Expression) = order match {
        case Zero => Digamma(expression)
        case _ => new Polygamma(order, expression)
    }

}

class Polygamma(val order: Natural, val of: Expression)
        extends Function {

    protected[this] def at(n: Number) = Polygamma(order, n)

    protected[this] def apply(expr: Expression) = Polygamma(order, expr)

    def isEmpty = false //TODO

    override def toString = s"Polygamma($order)($of)"

    protected[this] def derivative(at: Expression) = Polygamma(order.succ, at)

}

object Digamma
        extends ExpressionBuilder {

    def apply(n: Number): Number = Polygamma(Zero, n)

    protected[this] def create(expr: Expression) = new Digamma(expr)

    protected[this] def empty = ComplexInfinity

}

class Digamma(override val of: Expression)
        extends Polygamma(0, of) {

    override def toString = s"Digamma($of)"

}