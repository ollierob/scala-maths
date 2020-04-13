package net.ollie.maths.methods

import net.ollie.maths._
import net.ollie.maths.expressions.Univariate
import net.ollie.maths.functions.FunctionBuilder
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{Natural, Precision, Real}

/**
 * Evaluates a univariate expression at a value, using the derivatives at another value (whose value is, ideally, already explicitly known).
 * Created by Ollie on 12/01/14.
 *
 * @see http://mathworld.wolfram.com/TaylorSeries.html
 */
object RealTaylorSeries {

    def apply(expression: Univariate, at: Real, around: Real): Real = {
        new RealTaylorSeries(expression, at, around)(Real)
    }

    def apply(builder: FunctionBuilder, at: Real, around: Real): Real = {
        val x = Variable("$x")
        RealTaylorSeries(builder(x), at, around)
    }

}

private class RealTaylorSeries(val f: Univariate, val x: Real, val a: Real)(implicit conversion: NumberIdentityArithmetic[Real])
    extends Real
        with IterativelyEvaluated {

    def isEmpty = false //TODO?

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        val xMinusA = x - a
        var series: Real = Zero
        var fNDash: Univariate = f

        def next(n: Natural, precision: Precision): BigDecimal = {
            series += nthTerm(n)
            fNDash = fNDash.dx
            series.evaluate(precision)
        }

        def nthTerm(n: Natural): Real = fNDash(a) * (xMinusA ^ n) / (n !)

        implicit def convert(n: Constant): Real = conversion(n).get

    }

    override def toString = s"TaylorSeries($f, $x, $a)"

}

/**
 * Taylor series around zero.
 */
object RealMaclaurinSeries {

    def apply(builder: FunctionBuilder, at: Real): Real = {
        RealMaclaurinSeries(builder(Variable.temp), at)
    }

    def apply(expression: Univariate, at: Real): Real = RealTaylorSeries(expression, at, Zero)

}