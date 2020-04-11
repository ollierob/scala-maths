package net.ollie.maths.functions.hypergeometric

import net.ollie.maths._
import net.ollie.maths.expressions.{Composition, Expression}
import net.ollie.maths.numbers.{Infinity, Natural, Precision, Real}
import net.ollie.maths.numbers.complex.ComplexInfinity
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.methods.Integrate
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.functions.FunctionBuilder

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/PolygammaFunction.html
 */
trait Polygamma {

    def degree: Natural

    def of: Expression

    override def toString = s"Polygamma($degree)($of)"

}

object Polygamma {

    def apply(degree: Natural, expression: Expression): Expression = {
        degree match {
            case Zero => Digamma(expression)
            case _ => new PolygammaOf(degree, expression)
        }
    }

    def apply(degree: Natural, n: Constant): Constant with Polygamma = {
        Real(degree) match {
            case Some(re) => apply(degree, re)
            case _ => ???
        }
    }

    def apply(degree: Natural, re: Real): Real with Polygamma = {
        new RealPolygamma(degree, re)
    }

}

class PolygammaOf(val degree: Natural, val of: Expression)
        extends Composition
        with Polygamma {

    protected[this] def at(n: Constant) = Polygamma(degree, n)

    protected[this] def apply(expr: Expression) = Polygamma(degree, expr)

    def isEmpty = false //TODO

    protected[this] def derivative(at: Expression) = Polygamma(degree.succ, at)

}

class RealPolygamma(val degree: Natural, val of: Real)
        extends Real
        with Polygamma
        with CachedEvaluated {

    def isEmpty = false //TODO

    private lazy val integral: Real = {
        Integrate(t => (t ^ degree) * Exp(-of * t) / (1 - Exp(-t)), Zero, Infinity)
    }

    protected[this] def doEvaluate(precision: Precision) = integral.evaluate(precision)

}

object Digamma
        extends FunctionBuilder {

    def apply(n: Constant): Constant = Polygamma(Zero, n)

    def apply(re: Real) = Polygamma(Zero, re)

    protected[this] def create(expr: Expression) = new DigammaOf(expr)

    protected[this] def empty = ComplexInfinity

}

class DigammaOf(override val of: Expression)
        extends PolygammaOf(0, of) {

    override def toString = s"Digamma($of)"

}