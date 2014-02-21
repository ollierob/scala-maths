package net.ollie.maths.methods

import net.ollie.maths.{Expression, Univariate, Variable}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.Zero

/**
 * Real integrals.
 * Created by Ollie on 19/01/14.
 */
object Integrate {

    def apply(fn: Variable => Univariate, from: Real, to: Real)(implicit method: NumericalIntegrationMethod = SimpsonsIntegrationMethod): Real = {
        if (from >= to) Zero
        else method(fn, from, to)
    }

    def apply(integrand: Univariate, from: Real, to: Real)(implicit method: NumericalIntegrationMethod): Real = {
        if (from >= to) Zero
        else method(integrand, from, to)
    }

    def apply(integrand: Expression, wrt: Variable, from: Expression, to: Expression): DefiniteIntegral = {
        new DefiniteIntegralOf(integrand, wrt, from, to)
    }

    def apply(fn: Variable => Expression, from: Expression, to: Expression): DefiniteIntegral = {
        val v = Variable.random()
        Integrate(fn(v), v, from, to)
    }

    def apply(integrand: Expression, wrt: Variable): Integral = new IndefiniteIntegralOf(integrand, wrt)

    def apply(fn: Variable => Expression): Integral = {
        val v = Variable("$v")
        Integrate(fn(v), v)
    }

}

sealed trait Integral
        extends Expression {

    def integrand: Expression

    def variable: Variable

    def variables = integrand.variables

    override def toString = s"∫($integrand) d($variable)"

}

trait DefiniteIntegral
        extends Integral {

    def from: Expression

    def to: Expression

    def isEmpty = false //this.evaluate(SinglePrecision) == 0

    override def toString = s"∫($from:$to)($integrand) d($variable)"

}

trait NumericalIntegrationMethod {

    def apply(integrand: Univariate, from: Real, to: Real): DefiniteIntegral with Real = (from, to) match {
        case (_, Infinity) => toInfinity(integrand, from)
        case _ => finite(integrand, from, to)
    }

    def apply(fn: Variable => Univariate, from: Real, to: Real): DefiniteIntegral with Real = {
        val t = Variable("$t")
        apply(fn(t), from, to)
    }

    def finite(of: Univariate, from: Real, to: Real): DefiniteIntegral with Real

    def toInfinity(of: Univariate, from: Real): DefiniteIntegral with Real = ??? //new InfiniteIntegral(of, degree)(this)

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

            var N: Int = Math.max(4, startPrecision.digits)

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

            var n: Int = even(Math.max(4, startPrecision.digits))

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

class InfiniteIntegral(val integrand: Univariate, val from: Real)(implicit method: NumericalIntegrationMethod)
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

class DefiniteIntegralOf(val integrand: Expression, val variable: Variable, val from: Expression, val to: Expression)
        extends DefiniteIntegral {

    def df(x: Variable) = ???

    def toConstant = ???

    def replace(variables: Map[Variable, Expression]) = {
        Integrate(integrand.replace(variables), variable, from.replace(variables), to.replace(variables))
    }

    def unary_-() = Integrate(-integrand, variable, from, to)

}

class IndefiniteIntegralOf(val integrand: Expression, val variable: Variable)
        extends Integral {

    def df(x: Variable) = {
        if (x == variable) integrand
        else Integrate(integrand.df(x), variable)
    }

    def toConstant = ???

    def replace(variables: Map[Variable, Expression]) = Integrate(integrand.replace(variables), variable)

    def isEmpty = false

    def unary_-() = Expression.negate(this)

}