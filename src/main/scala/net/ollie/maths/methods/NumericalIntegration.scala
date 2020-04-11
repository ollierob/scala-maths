package net.ollie.maths.methods

import net.ollie.maths.Variable
import net.ollie.maths.expressions.Univariate
import net.ollie.maths.numbers.{Infinity, Natural, Precision, Real}
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 19/01/14.
 */
trait NumericalIntegrationMethod {

    def apply(integrand: Univariate, from: Real, to: Real): DefiniteIntegral with Real = (from, to) match {
        case (_, Infinity) => toInfinity(integrand, from)
        case _ => finite(integrand, from, to)
    }

    def apply(fn: Variable => Univariate, from: Real, to: Real): DefiniteIntegral with Real = {
        val t = Variable.virtual
        apply(fn(t), from, to)
    }

    def finite(of: Univariate, from: Real, to: Real): DefiniteIntegral with Real

    def toInfinity(of: Univariate, from: Real): DefiniteIntegral with Real = new InfiniteIntegral(of, from)(this)

    def betweenInfinities(of: Univariate): DefiniteIntegral = ???

}

object TrapezoidalIntegrationMethod
        extends NumericalIntegrationMethod {

    def finite(of: Univariate, from: Real, to: Real): DefiniteIntegral with Real = new ProperIntegral(of, from, to)

    private class ProperIntegral(val integrand: Univariate, val from: Real, val to: Real)
            extends DefiniteIntegral
            with Real
            with IterativelyEvaluated {

        def variable = integrand.variable

        override def variables = super[Real].variables

        val delta = to - from

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            var N: Int = Math.max(4, startPrecision.digits.toInt.get)

            def next(nth: Natural, precision: Precision): BigDecimal = {
                val h = delta / N
                val terms = split(h)
                var totalArea: Real = Zero
                for (i <- 1 to N) {
                    val t: Real = integrand(terms(i - 1))(Real) + integrand(terms(i))(Real)
                    totalArea += t
                }
                totalArea *= delta / (2 * N)
                N += 1
                totalArea.approximatelyEvaluate(precision)
            }

            private def split(h: Real): IndexedSeq[Real] = {
                var n: Int = -1
                Iterable.fill(N + 1)({
                    n = n + 1
                    (from + (n * h)).abs
                }).toIndexedSeq
            }

        }

    }

}

/**
 * @see http://mathworld.wolfram.com/SimpsonsRule.html
 */
object SimpsonsIntegrationMethod
        extends NumericalIntegrationMethod {

    def finite(of: Univariate, from: Real, to: Real): DefiniteIntegral with Real = new ProperIntegral(of, from, to)

    private class ProperIntegral(val integrand: Univariate, val from: Real, val to: Real)
            extends DefiniteIntegral
            with Real
            with IterativelyEvaluated {

        def variable = integrand.variable

        override def variables = super[Real].variables

        val interval = to - from

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            var n: Int = even(Math.max(4, startPrecision.digits.toInt.get))

            def next(nth: Natural, precision: Precision) = {
                val h = interval / n
                val terms = split(h)
                var totalArea: Real = integrand(terms(0))(Real) + integrand(terms.last)(Real)
                for (i <- 1 to n - 1) {
                    totalArea += integrand(terms(i))(Real) * (if (i % 2 == 0) 2 else 4)
                }
                totalArea *= h / 3
                n += 2
                val evaluated = totalArea.approximatelyEvaluate(precision)
                evaluated
            }

            private def split(h: Real): IndexedSeq[Real] = {
                var i: Int = -1
                Iterable.fill(n + 1)({
                    i = i + 1
                    (from + (i * h))
                }).toIndexedSeq
            }

            private def even(i: Int): Int = if (i % 2 == 0) i else i + 1

        }

    }

}
