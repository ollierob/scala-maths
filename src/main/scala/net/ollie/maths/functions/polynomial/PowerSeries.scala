package net.ollie.maths.functions.polynomial

import net.ollie.maths.Constant
import net.ollie.maths.expressions.{Expression, NegatedExpression, Univariate}
import net.ollie.maths.numbers.Natural

trait PowerSeries
    extends Expression {

    type Coefficient <: Constant

    def of: Expression

    def coefficient(power: Natural): Coefficient

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

    override type Coefficient = Constant

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

trait PowerSeriesCoefficients[C <: Constant] {

    def value(n: Natural): C

    def isEmpty: Boolean

    def degree: Option[Natural]

}

object PowerSeriesCoefficients {

    def transformEach[F <: Constant, T <: Constant](c: PowerSeriesCoefficients[F], each: Function2[Natural, F, T]): PowerSeriesCoefficients[T] = {
        new TransformedPowerSeriesCoefficients(c, each)
    }

}

class TransformedPowerSeriesCoefficients[F <: Constant, T <: Constant](val delegate: PowerSeriesCoefficients[F], val each: Function2[Natural, F, T])
    extends PowerSeriesCoefficients[T] {

    override def value(n: Natural): T = {
        each(n, delegate.value(n))
    }

    override def isEmpty = delegate.isEmpty

    override def degree = delegate.degree

}