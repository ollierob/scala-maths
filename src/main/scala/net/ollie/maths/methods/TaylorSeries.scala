package net.ollie.maths.methods

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import net.ollie.maths._
import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.numbers.{NaturalNumber, Precision, RealNumber, Zero}

/**
 * Evaluates a univariate expression at a value, using the derivatives at another value (whose value is, ideally, already explicitly known).
 * Created by Ollie on 12/01/14.
 * @see http://mathworld.wolfram.com/TaylorSeries.html
 */
object TaylorSeries {

    def apply(expression: Univariate, at: RealNumber, around: RealNumber): RealNumber = {
        new TaylorSeries(expression, at, around)
    }

    def apply(builder: ExpressionBuilder, at: RealNumber, around: RealNumber): RealNumber = {
        val x = Variable("$x")
        TaylorSeries(builder(x), at, around)
    }

}

private class TaylorSeries(val f: Univariate, val x: RealNumber, val a: RealNumber)(implicit conversion: NumberIdentityArithmetic[RealNumber])
        extends RealNumber
        with IterativelyEvaluated {

    def isEmpty = false //TODO?

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        val xMinusA = x - a
        val terms: mutable.Buffer[RealNumber] = new ListBuffer[RealNumber]()
        var fNDash: Univariate = f

        def next(n: NaturalNumber, precision: Precision): BigDecimal = {
            terms += nthTerm(n)
            println(s"$fNDash at $a => " + fNDash(a))
            fNDash = fNDash.dx
            println(s"Nth $n => $terms => " + terms.map(_.approximatelyEvaluate(precision)).sum)
            terms.map(_.approximatelyEvaluate(precision)).sum
        }

        def nthTerm(n: NaturalNumber): RealNumber = fNDash(a) * (xMinusA ^ n) / (n !)

        implicit def convert(n: Number): RealNumber = conversion.convert(n).get

    }

    override def toString = s"TaylorSeries($f, $x, $a)"

}

/**
 * Taylor series around zero.
 */
object MaclaurinSeries {

    def apply(builder: ExpressionBuilder, at: RealNumber): RealNumber = {
        val x = Variable("$x")
        MaclaurinSeries(builder(x), at)
    }

    def apply(expression: Univariate, at: RealNumber): RealNumber = TaylorSeries(expression, at, Zero)

}