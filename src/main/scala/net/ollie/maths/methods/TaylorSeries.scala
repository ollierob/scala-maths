package net.ollie.maths.methods

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import net.ollie.maths._
import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.numbers.{Natural, Precision, Real}
import net.ollie.maths.numbers.constants.Zero

/**
 * Evaluates a univariate expression at a value, using the derivatives at another value (whose value is, ideally, already explicitly known).
 * Created by Ollie on 12/01/14.
 * @see http://mathworld.wolfram.com/TaylorSeries.html
 */
object TaylorSeries {

    def apply(expression: Univariate, at: Real, around: Real): Real = {
        new TaylorSeries(expression, at, around)(Real)
    }

    def apply(builder: ExpressionBuilder, at: Real, around: Real): Real = {
        val x = Variable("$x")
        TaylorSeries(builder(x), at, around)
    }

}

private class TaylorSeries(val f: Univariate, val x: Real, val a: Real)(implicit conversion: NumberIdentityArithmetic[Real])
        extends Real
        with IterativelyEvaluated {

    def isEmpty = false //TODO?

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        val xMinusA = x - a
        val terms: mutable.Buffer[Real] = new ListBuffer[Real]()
        var fNDash: Univariate = f

        def next(n: Natural, precision: Precision): BigDecimal = {
            terms += nthTerm(n)
            fNDash = fNDash.dx
            terms.map(_.approximatelyEvaluate(precision)).sum
        }

        def nthTerm(n: Natural): Real = fNDash(a) * (xMinusA ^ n) / (n !)

        implicit def convert(n: Number): Real = conversion(n).get

    }

    override def toString = s"TaylorSeries($f, $x, $a)"

}

/**
 * Taylor series around zero.
 */
object MaclaurinSeries {

    def apply(builder: ExpressionBuilder, at: Real): Real = {
        val x = Variable("$x")
        MaclaurinSeries(builder(x), at)
    }

    def apply(expression: Univariate, at: Real): Real = TaylorSeries(expression, at, Zero)

}