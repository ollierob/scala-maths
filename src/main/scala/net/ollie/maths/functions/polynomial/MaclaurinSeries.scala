package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Constant, Variable}
import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.numbers.Natural

trait MaclaurinSeries
    extends PowerSeries with Univariate {

    override def of: Variable

    override def variables = super[Univariate].variables

    override def variable = of

    override def toConstant = None

    override def unary_-(): MaclaurinSeries = new NegatedMaclaurinSeries(this)

}

object MaclaurinSeries {

    def apply[C <: Constant](x: Variable, generator: PowerSeriesCoefficients[C]): MaclaurinSeries = new GeneratedMaclaurinSeries(x, generator)

}

private class NegatedMaclaurinSeries[S <: MaclaurinSeries](override val series: S)
    extends NegatedPowerSeries(series) with MaclaurinSeries {

    override def of = series.of

    override def dx = -(series.dx)

    override def unary_-() = series

}

private class GeneratedMaclaurinSeries[C <: Constant](val x: Variable, val generator: PowerSeriesCoefficients[C])
    extends MaclaurinSeries {

    override def isEmpty = generator.isEmpty

    override def of = x

    override def dx = ???

    override def coefficient(power: Natural) = generator.value(power)

    override def replace(variables: Map[Variable, Expression]) = ???

}