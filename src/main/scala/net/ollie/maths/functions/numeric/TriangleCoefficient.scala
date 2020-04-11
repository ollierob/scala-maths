package net.ollie.maths.functions.numeric

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{Represented, SymmetricTrivariateFunction}
import net.ollie.maths.functions.hypergeometric.Gamma
import net.ollie.maths.numbers.{Natural, PositiveReal, Precision}

/**
 * Created by Ollie on 26/01/14.
 */
object TriangleCoefficient
        extends SymmetricTrivariateFunction[Natural, PositiveReal] {

    def apply(a: Expression, b: Expression, c: Expression): Expression with TriangleCoefficient = {
        (a, b, c) match {
            case (i1: Natural, i2: Natural, i3: Natural) => apply(i1, i2, i3)
            case _ => new TriangleCoefficientOf(a, b, c)
        }
    }

    def apply(a: Natural, b: Natural, c: Natural) = {
        new TriangleCoefficientOfNatural(a, b, c)
    }

}

trait TriangleCoefficient {

    def a: Expression

    def b: Expression

    def c: Expression

    def isEmpty = false

    override def toString = s"Triangle($a, $b, $c)"

}

private class TriangleCoefficientOf(val a: Expression, val b: Expression, val c: Expression)
        extends Represented
        with TriangleCoefficient {

    def representation = Gamma(a + b - c + 1) * Gamma(a - b + c + 1) * Gamma(b + c - a + 1) / Gamma(a + b + c + 2)

    override def isEmpty = super[TriangleCoefficient].isEmpty

}

class TriangleCoefficientOfNatural(val a: Natural, val b: Natural, val c: Natural)
        extends PositiveReal
        with TriangleCoefficient
        with CachedEvaluated {

    require(a + b >= c)
    require(a + c >= b)
    require(b + c >= a)

    private lazy val f = ((a + b - c).abs.!) * ((a - b + c).abs.!) * ((b + c - a).abs.!) / ((a + b + c + 1).abs.!)

    protected[this] def doEvaluate(precision: Precision) = f.evaluate(precision)

}