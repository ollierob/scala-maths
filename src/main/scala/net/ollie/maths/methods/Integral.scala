package net.ollie.maths.methods

import net.ollie.maths.{Expression, Univariate, Variable}
import net.ollie.maths.numbers._

/**
 * Real integrals.
 * Created by Ollie on 19/01/14.
 */
object Integral {

    def apply(fn: Variable => Univariate, from: RealNumber, to: RealNumber)(implicit method: NumericalIntegrationMethod): RealNumber = {
        if (from >= to) Zero
        else method(fn, from, to)
    }

    def apply(expression: Univariate, from: RealNumber, to: RealNumber)(implicit method: NumericalIntegrationMethod): RealNumber = {
        if (from >= to) Zero
        else method(expression, from, to)
    }

}

trait Integral
        extends RealNumber {

    def of: Expression

    def variable: Variable

    override def toString = s"∫($of) d($variable)"

}

trait DefiniteIntegral
        extends Integral
        with RealNumber {

    def of: Univariate

    def variable = of.variable

    def from: RealNumber

    def to: RealNumber

    def isEmpty = false //this.evaluate(SinglePrecision) == 0

    override def toString = s"∫($from:$to)($of) d($variable)"

}

trait NumericalIntegrationMethod {

    def apply(of: Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral = (from, to) match {
        case (_, Infinity) => toInfinity(of, from)
        case _ => finite(of, from, to)
    }

    def apply(fn: Variable => Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral = {
        val t = Variable("$t")
        apply(fn(t), from, to)
    }

    def finite(of: Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral

    def toInfinity(of: Univariate, from: RealNumber): DefiniteIntegral = ??? //new InfiniteIntegral(of, from)(this)

    def betweenInfinities(of: Univariate): DefiniteIntegral = ???

}

object TrapezoidalIntegrationMethod
        extends NumericalIntegrationMethod {

    def finite(of: Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral = new ProperIntegral(of, from, to)

    private class ProperIntegral(val of: Univariate, val from: RealNumber, val to: RealNumber)
            extends DefiniteIntegral
            with IterativelyEvaluated {

        val delta = to - from

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            var N: Int = Math.max(4, startPrecision.value)

            def next(nth: NaturalNumber, precision: Precision): BigDecimal = {
                val h = delta / N
                val terms = split(h)
                var totalArea: RealNumber = Zero
                for (i <- 1 to N) {
                    val t = of(terms(i - 1)) + of(terms(i))
                    totalArea += t
                }
                totalArea *= delta / (2 * N)
                N += 1
                totalArea.approximatelyEvaluate(precision)
            }

            private def split(h: RealNumber): IndexedSeq[RealNumber] = {
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

    def finite(of: Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral = new ProperIntegral(of, from, to)

    private class ProperIntegral(val of: Univariate, val from: RealNumber, val to: RealNumber)
            extends DefiniteIntegral
            with IterativelyEvaluated {

        val interval = to - from

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            var n: Int = even(Math.max(4, startPrecision.value))

            def next(nth: NaturalNumber, precision: Precision) = {
                val h = interval / n
                val terms = split(h)
                var totalArea: RealNumber = of(terms(0)) + of(terms.last)
                for (i <- 1 to n - 1) {
                    totalArea += of(terms(i)) * (if (i % 2 == 0) 2 else 4)
                }
                totalArea *= h / 3
                n += 2
                val evaluated = totalArea.approximatelyEvaluate(precision)
                evaluated
            }

            private def split(h: RealNumber): IndexedSeq[RealNumber] = {
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

class InfiniteIntegral(val of: Univariate, val from: RealNumber)(implicit method: NumericalIntegrationMethod)
        extends DefiniteIntegral
        with IterativelyEvaluated {

    final def to = Infinity

    protected[this] def upperLimit(n: NaturalNumber): RealNumber = 10 * (n.succ)

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

        def next(nth: NaturalNumber, precision: Precision) = Integral(of, from, upperLimit(nth))(method).evaluate(precision)

    }

}