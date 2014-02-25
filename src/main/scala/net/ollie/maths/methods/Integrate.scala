package net.ollie.maths.methods

import net.ollie.maths.{Expression, Univariate, Variable}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.Zero

/**
 * Real integrals.
 * Created by Ollie on 19/01/14.
 */
object Integrate {

    def apply(fn: Variable => Univariate, from: Real, to: Real)
            (implicit method: NumericalIntegrationMethod = SimpsonsIntegrationMethod): Real = {
        if (from >= to) Zero
        else method(fn, from, to)
    }

    def apply(integrand: Univariate, from: Real, to: Real)
            (implicit method: NumericalIntegrationMethod): Real = {
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