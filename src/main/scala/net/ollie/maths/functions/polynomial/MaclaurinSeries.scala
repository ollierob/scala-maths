package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
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

}

private class NegatedMaclaurinSeries[S <: MaclaurinSeries](override val series: S)
    extends NegatedPowerSeries(series) with MaclaurinSeries {

    override def of = series.of

    override def dx = -(series.dx)

    override def unary_-() = series

}

private class GeneratedMaclaurinSeries(val x: Variable)
    extends MaclaurinSeries {

    override def of = x

    override def dx = ???

    override def coefficient(power: Natural) = ???

    override def replace(variables: Map[Variable, Expression]) = ???

    override def isEmpty: Boolean = ???

}