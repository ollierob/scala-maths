package net.ollie.maths.functions.numeric

import net.ollie.maths.{Univariate, Variable}
import net.ollie.maths.numbers.{Natural, Precision, Real}
import net.ollie.maths.methods.{EvaluationIterator, IterativelyEvaluated}

/**
 * Created by Ollie on 03/03/14.
 */
object Limit {

    def fromAbove(function: Variable => Univariate, limit: Real): Real = {
        apply(function, limit, true)
    }

    def fromBelow(function: Variable => Univariate, limit: Real): Real = {
        apply(function, limit, false)
    }

    def apply(function: Variable => Univariate, limit: Real, fromAbove: Boolean): Real = {
        val x = Variable.virtual
        new RealLimit(function(x), limit, fromAbove)
    }

}

class RealLimit(val expression: Univariate, val limit: Real, val fromAbove: Boolean)
        extends Real
        with IterativelyEvaluated {

    def isEmpty = expression.isEmpty //TODO

    private def ±(term: Real): Real = if (fromAbove) term else -term

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        protected[this] def nthGuess(n: Natural): Real = limit + ±(10 ^ (-(n + startPrecision.digits)))

        def next(nth: Natural, precision: Precision): BigDecimal = {
            val current: Real = nthGuess(nth)
            val evaluated: Real = expression(current)(Real)
            evaluated.evaluate(precision)
        }

    }

}