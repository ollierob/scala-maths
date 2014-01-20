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

    //    def apply(expression: Differentiable, at: RealNumber, around: RealNumber): Option[RealNumber] = expression match {
    //        case _ if expression.isEmpty => Some(Zero)
    //        case n: RealNumber => Some(n)
    //        case du: DifferentiableUnivariate => Some(TaylorSeries(du, at, around))
    //        case _ if expression.variables.size == 1 => Some(TaylorSeries(new DifferentiableUnivariateWrapper(expression), at, around))
    //        case _ => None
    //    }

    def apply(expression: Univariate, at: RealNumber, around: RealNumber): RealNumber = {
        new TaylorSeries(expression, at, around)
    }

    def apply(builder: ExpressionBuilder, at: RealNumber, around: RealNumber): RealNumber = {
        val x = Variable("$x")
        TaylorSeries(builder(x), at, around)
    }

}

private class TaylorSeries(f: Univariate, val x: RealNumber, a: RealNumber)(implicit conversion: IdentityArithmetic[Number, RealNumber])
        extends RealNumber
        with IterativelyEvaluated {

    def isEmpty = false //TODO?

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        val xMinusA = x - a
        val terms: mutable.Buffer[RealNumber] = new ListBuffer[RealNumber]()
        var fNDash: Univariate = f

        def next(n: NaturalNumber, precision: Precision): BigDecimal = {
            terms += nthTerm(n)
            fNDash = fNDash.dx
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