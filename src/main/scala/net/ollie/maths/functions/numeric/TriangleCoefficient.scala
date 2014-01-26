package net.ollie.maths.functions.numeric

import net.ollie.maths.Expression
import net.ollie.maths.functions.{Represented, TrivariateFunction}
import net.ollie.maths.functions.hypergeometric.Gamma
import net.ollie.maths.numbers.{Natural, PositiveReal, Precision}

/**
 * Created by Ollie on 26/01/14.
 */
object TriangleCoefficient
        extends TrivariateFunction[Natural, Natural, Natural, PositiveReal] {

    def apply(a: Expression, b: Expression, c: Expression): Expression = (a, b, c) match {
        case (i1: Natural, i2: Natural, i3: Natural) => apply(i1, i2, i3)
        case _ => new TriangleCoefficient(a, b, c)
    }

    def apply(a: Natural, b: Natural, c: Natural): PositiveReal = new TriangleCoefficientOf(a, b, c)

}

private class TriangleCoefficient(val a: Expression, val b: Expression, val c: Expression)
        extends Represented {

    protected[this] def f: Expression = Gamma(a + b - c + 1) * Gamma(a - b + c + 1) * Gamma(b + c - a + 1) / Gamma(a + b + c + 2)

    override def toString = s"Triangle($a, $b, $c)"

}

private class TriangleCoefficientOf(val a: Natural, val b: Natural, val c: Natural)
        extends PositiveReal {

    require(a + b >= c)
    require(a + c >= b)
    require(b + c >= a)

    private lazy val f = ((a + b - c).abs.!) * ((a - b + c).abs.!) * ((b + c - a).abs.!) / ((a + b + c + 1).abs.!)

    protected[this] def eval(precision: Precision) = f.evaluate(precision)

    private lazy val empty = a.isEmpty && b.isEmpty && c.isEmpty

    def isEmpty = empty

    override def toString = s"Triangle($a, $b, $c)"

}