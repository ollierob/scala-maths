package net.ollie.maths.methods

import net.ollie.maths.{Expression, Univariate, Variable}
import net.ollie.maths.numbers._

/**
 * Real integrals.
 * Created by Ollie on 19/01/14.
 */
object Integral {

    def apply(fn: Variable => Univariate, from: Real, to: Real)(implicit method: NumericalIntegrationMethod): Real = {
        if (from >= to) Zero
        else method(fn, from, to)
    }

    def apply(expression: Univariate, from: Real, to: Real)(implicit method: NumericalIntegrationMethod): Real = {
        if (from >= to) Zero
        else method(expression, from, to)
    }

}

trait Integral
        extends Real {

    def of: Expression

    def variable: Variable

    override def toString = s"∫($of) d($variable)"

}

trait DefiniteIntegral
        extends Integral
        with Real {

    def of: Univariate

    def variable = of.variable

    def from: Real

    def to: Real

    def isEmpty = false //this.evaluate(SinglePrecision) == 0

    override def toString = s"∫($from:$to)($of) d($variable)"

}

trait NumericalIntegrationMethod {

    def apply(of: Univariate, from: Real, to: Real): DefiniteIntegral = (from, to) match {
        case (_, Infinity) => toInfinity(of, from)
        case _ => finite(of, from, to)
    }

    def apply(fn: Variable => Univariate, from: Real, to: Real): DefiniteIntegral = {
        val t = Variable("$t")
        apply(fn(t), from, to)
    }

    def finite(of: Univariate, from: Real, to: Real): DefiniteIntegral

    def toInfinity(of: Univariate, from: Real): DefiniteIntegral = ??? //new InfiniteIntegral(of, from)(this)

    def betweenInfinities(of: Univariate): DefiniteIntegral = ???

}

object TrapezoidalIntegrationMethod
        extends NumericalIntegrationMethod {

    def finite(of: Univariate, from: Real, to: Real): DefiniteIntegral = new ProperIntegral(of, from, to)

    private class ProperIntegral(val of: Univariate, val from: Real, val to: Real)
            extends DefiniteIntegral
            with IterativelyEvaluated {

        val delta = to - from

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            var N: Int = Math.max(4, startPrecision.value)

            def next(nth: Natural, precision: Precision): BigDecimal = {
                val h = delta / N
                val terms = split(h)
                var totalArea: Real = Zero
                for (i <- 1 to N) {
                    val t = of(terms(i - 1)) + of(terms(i))
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

    def finite(of: Univariate, from: Real, to: Real): DefiniteIntegral = new ProperIntegral(of, from, to)

    private class ProperIntegral(val of: Univariate, val from: Real, val to: Real)
            extends DefiniteIntegral
            with IterativelyEvaluated {

        val interval = to - from

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            var n: Int = even(Math.max(4, startPrecision.value))

            def next(nth: Natural, precision: Precision) = {
                val h = interval / n
                val terms = split(h)
                var totalArea: Real = of(terms(0)) + of(terms.last)
                for (i <- 1 to n - 1) {
                    totalArea += of(terms(i)) * (if (i % 2 == 0) 2 else 4)
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

class InfiniteIntegral(val of: Univariate, val from: Real)(implicit method: NumericalIntegrationMethod)
        extends DefiniteIntegral
        with IterativelyEvaluated {

    final def to = Infinity

    protected[this] def upperLimit(n: Natural): Real = 10 * (n.succ)

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

        def next(nth: Natural, precision: Precision) = Integral(of, from, upperLimit(nth))(method).evaluate(precision)

    }

}