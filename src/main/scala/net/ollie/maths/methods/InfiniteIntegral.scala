package net.ollie.maths.methods

import net.ollie.maths.expressions.Univariate
import net.ollie.maths.numbers.{Infinity, Natural, Precision, Real}

class InfiniteIntegral(val integrand: Univariate, val from: Real)
        (implicit method: NumericalIntegrationMethod)
        extends DefiniteIntegral
        with Real
        with IterativelyEvaluated {

    def variable = integrand.variable

    override def variables = super[Real].variables

    final def to = Infinity

    protected[this] def upperLimit(n: Natural): Real = 10 * (n.succ)

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

        def next(nth: Natural, precision: Precision) = Integrate(integrand, from, upperLimit(nth))(method).evaluate(precision)

    }

}
