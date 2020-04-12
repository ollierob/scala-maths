package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Constant, Variable}
import net.ollie.maths.expressions.{Expression, NegatedExpression, Univariate}
import net.ollie.maths.numbers.Natural

trait PowerSeries
    extends Expression {

    def of: Expression

    def coefficient(power: Natural): Constant

    override def unary_-(): PowerSeries = new NegatedPowerSeries(this)

}

trait UnivariateCenteredPowerSeries
    extends PowerSeries with Univariate {

    def center: Constant

    override def of = variable - center

    override def unary_-(): UnivariateCenteredPowerSeries = new NegatedUnivariateCenteredPowerSeries(this)

}

class NegatedPowerSeries[P <: PowerSeries](val series: P)
    extends NegatedExpression(series) with PowerSeries {

    override def of: Expression = series.of

    override def coefficient(power: Natural) = -series.coefficient(power)

    override def unary_-() = series

}

class NegatedUnivariateCenteredPowerSeries[P <: UnivariateCenteredPowerSeries](override val series: P)
    extends NegatedPowerSeries(series) with UnivariateCenteredPowerSeries {

    override def center = series.center

    override def variable = series.variable

    override def variables = super.variables

    override def dx = -(series.dx)

    override def unary_-() = series

}