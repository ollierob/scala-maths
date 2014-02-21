package net.ollie.maths.functions.hypergeometric

import net.ollie.maths._
import net.ollie.maths.numbers.{Infinity, Precision, Real, Natural}
import net.ollie.maths.numbers.complex.ComplexInfinity
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.methods.Integrate
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.functions.FunctionBuilder

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/PolygammaFunction.html
 */
object Polygamma {

    def apply(order: Natural, expression: Expression): Expression = order match {
        case Zero => Digamma(expression)
        case _ => new PolygammaOf(order, expression)
    }

    def apply(order: Natural, n: Number): Number with Polygamma = Real(order) match {
        case Some(re) => apply(order, re)
        case _ => ???
    }

    def apply(order: Natural, re: Real): Real with Polygamma = new RealPolygamma(order, re)

}

trait Polygamma {

    def order: Natural

    def of: Expression

    override def toString = s"Polygamma($order)($of)"

}

class PolygammaOf(val order: Natural, val of: Expression)
        extends Function
        with Polygamma {

    protected[this] def at(n: Number) = Polygamma(order, n)

    protected[this] def apply(expr: Expression) = Polygamma(order, expr)

    def isEmpty = false //TODO

    protected[this] def derivative(at: Expression) = Polygamma(order.succ, at)

}

class RealPolygamma(val order: Natural, val of: Real)
        extends Real
        with Polygamma {

    def isEmpty = false //TODO

    private lazy val integral: Real = Integrate(t => (t ^ order) * Exp(-of * t) / (1 - Exp(-t)), Zero, Infinity)

    protected[this] def doEvaluate(precision: Precision) = integral.evaluate(precision)

}

object Digamma
        extends FunctionBuilder {

    def apply(n: Number): Number = Polygamma(Zero, n)

    def apply(re: Real) = Polygamma(Zero, re)

    protected[this] def create(expr: Expression) = new DigammaOf(expr)

    protected[this] def empty = ComplexInfinity

}

class DigammaOf(override val of: Expression)
        extends PolygammaOf(0, of) {

    override def toString = s"Digamma($of)"

}