package net.ollie.maths.functions.numeric

import net.ollie.maths.expressions.Expression
import net.ollie.maths.numbers.{Natural, Precision, Real}
import net.ollie.maths.functions.{BivariateFunction, Represented}
import net.ollie.maths.numbers.combinatorial.BinomialCoefficient._
import net.ollie.maths.functions.hypergeometric.Gamma
import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 02/03/14.
 * @see http://mathworld.wolfram.com/RisingFactorial.html
 */
object RisingFactorial
        extends BivariateFunction[Real, Natural, Real] {

    def apply(x: Real, n: Natural): Real with RisingFactorial = {
        new RealRisingFactorial(x, n)
    }

    def apply(x: Natural, n: Natural): Natural with RisingFactorial = {
        new NaturalRisingFactorial(x, n)
    }

    def apply(x: Expression, n: Natural): RisingFactorial = x.toConstant match {
        case Some(m: Natural) => apply(m, n)
        case Some(re: Real) => apply(re, n)
        case _ => new RisingFactorialOf(x, n)
    }

}

trait RisingFactorial
        extends Expression {

    def of: Expression

    def order: Natural

    override def toString = s"RisingFactorial($of, $order)"

}

private class RealRisingFactorial(val of: Real, val order: Natural)
        extends Real
        with RisingFactorial {

    def isEmpty = of.isEmpty && order.isPositive

    private lazy val evaluated: Real = Gamma(of + order) / Gamma(of)

    def evaluate(precision: Precision) = evaluated.evaluate(precision)

}

private class NaturalRisingFactorial(val of: Natural, val order: Natural)
        extends Natural
        with RisingFactorial {

    private lazy val evaluator = {
        if (order.isEmpty) One
        else {
            val xpn: Natural = of + order - 1
            (order !) * (xpn choose order)
        }
    }

    override def evaluate = evaluator.evaluate

    override def toString = s"RisingFactorial($of, $order)"

}

private class RisingFactorialOf(val of: Expression, val order: Natural)
        extends RisingFactorial
        with Represented {

    def representation = Gamma(of + order) / Gamma(of)

}

/**
 * @see http://mathworld.wolfram.com/FallingFactorial.html
 */
object FallingFactorial
        extends BivariateFunction[Real, Natural, Real] {

    def apply(x: Real, n: Natural): Real with FallingFactorial = {
        new RealFallingFactorial(x, n)
    }

    def apply(x: Natural, n: Natural): Natural with FallingFactorial = {
        new NaturalFallingFactorial(x, n)
    }

    def apply(x: Expression, n: Natural): FallingFactorial = {
        x.toConstant match {
            case Some(m: Natural) => apply(m, n)
            case Some(re: Real) => apply(re, n)
            case _ => new FallingFactorialOf(x, n)
        }
    }

}

trait FallingFactorial
        extends Expression {

    def of: Expression

    def order: Natural

    override def toString = s"FallingFactorial($of, $order)"

}

private class RealFallingFactorial(val of: Real, val order: Natural)
        extends Real
        with FallingFactorial {

    private lazy val evaluator = Gamma(of + 1) / Gamma(of - order + 1)

    def isEmpty = evaluator.isEmpty

    def evaluate(precision: Precision) = evaluator.evaluate(precision)

}

private class NaturalFallingFactorial(val of: Natural, val order: Natural)
        extends Natural
        with FallingFactorial {

    private lazy val repr = (order !) * (of choose order)

    def evaluate = repr.evaluate

    override def toString = s"FallingFactorial($of, $order)"

}

private class FallingFactorialOf(val of: Expression, val order: Natural)
        extends Represented
        with FallingFactorial {

    def representation = Gamma(of + 1) / Gamma(of - order + 1)

}