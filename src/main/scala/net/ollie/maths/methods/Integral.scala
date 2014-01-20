package net.ollie.maths.methods

import net.ollie.maths.{Expression, Univariate, Variable}
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 19/01/14.
 */
object Integral {

    val DEFAULT_INTEGRATION_METHOD: NumericalIntegrationMethod = SimpsonsIntegrationMethod

    def apply(fn: Variable => Univariate, from: RealNumber, to: RealNumber)(implicit method: NumericalIntegrationMethod = DEFAULT_INTEGRATION_METHOD): RealNumber = {
        integrate(build(fn), from, to)
    }

    def integrate(expression: Univariate, from: RealNumber, to: RealNumber)(implicit method: NumericalIntegrationMethod = DEFAULT_INTEGRATION_METHOD): RealNumber = (from, to) match {
        case (MinusInfinity, Infinity) => ???
        case (_, Infinity) => toInfinity(expression, from)
        case (MinusInfinity, _) => ???
        case _ => proper(expression, from, to)
    }

    def toInfinity(ex: Univariate, from: RealNumber)(implicit method: NumericalIntegrationMethod = DEFAULT_INTEGRATION_METHOD): RealNumber = {
        require(isProper(from))
        new UpperInfiniteIntegral(ex, from)
    }

    def proper(ex: Univariate, from: RealNumber, to: RealNumber)(implicit method: NumericalIntegrationMethod = DEFAULT_INTEGRATION_METHOD): RealNumber = {
        require(isProper(from) && isProper(to))
        if (to <= from) Zero
        else method(ex, from, to)
    }

    private implicit def build(fn: Variable => Univariate): Univariate = {
        val x = new Variable("$x")
        fn(x)
    }

    private def isProper(re: RealNumber): Boolean = !re.isInstanceOf[Infinite]

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

    def from: RealNumber

    def to: RealNumber

    override def toString = s"∫($from:$to)($of) d($variable)"

}

private class UpperInfiniteIntegral(val of: Univariate, val from: RealNumber)(implicit method: NumericalIntegrationMethod)
        extends DefiniteIntegral {

    def isEmpty = of.isEmpty

    def variable = of.variable

    def to = Infinity

    private val t = Variable("$t")

    private lazy val transformed = Integral.proper(of(1 / t) / (t ^ 2), Zero, 1 / from)

    protected[this] def eval(precision: Precision) = transformed.evaluate(precision)

}

trait NumericalIntegrationMethod {

    def apply(of: Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral

}

object TrapezoidalIntegrationMethod
        extends NumericalIntegrationMethod {

    def apply(of: Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral = new ProperIntegral(of, from, to)

    private class ProperIntegral(val of: Univariate, val from: RealNumber, val to: RealNumber)
            extends DefiniteIntegral
            with IterativelyEvaluated {

        val delta = to - from

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            var N: Int = Math.max(1, startPrecision.value)

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

        def isEmpty = of.isEmpty

        def variable = of.variable

    }

}

/**
 * @see http://mathworld.wolfram.com/SimpsonsRule.html
 */
object SimpsonsIntegrationMethod
        extends NumericalIntegrationMethod {

    def apply(of: Univariate, from: RealNumber, to: RealNumber): DefiniteIntegral = new ProperIntegral(of, from, to)

    private class ProperIntegral(val of: Univariate, val from: RealNumber, val to: RealNumber)
            extends DefiniteIntegral
            with IterativelyEvaluated {

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

            val delta = to - from
            var n: Int = even(Math.max(2, startPrecision.value))

            def next(nth: NaturalNumber, precision: Precision) = {
                val h = delta / n
                val terms = split(h)
                var totalArea: RealNumber = of(terms(0)) + of(terms.last)
                for (i <- 1 to n - 1) {
                    totalArea += of(terms(i)) * (if (i % 2 == 0) 2 else 4)
                }
                totalArea *= h / 3
                n += 2
                totalArea.approximatelyEvaluate(precision)
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

        def isEmpty = false

        def variable = of.variable

    }

}