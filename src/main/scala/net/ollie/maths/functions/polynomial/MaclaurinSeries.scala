package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.{Constant, MultiplicationArithmetic, Variable}

trait MaclaurinSeries
    extends PowerSeries with Univariate {

    override def of: Variable

    override def variables = super[Univariate].variables

    override def variable = of

    override def toConstant = None

    override def replace(variables: Map[Variable, Expression]): Expression = ??? //TODO

    override def unary_-(): MaclaurinSeries = new NegatedMaclaurinSeries(this)

}

object MaclaurinSeries {

    def apply[C <: Constant](x: Variable, generator: PowerSeriesCoefficients[C])(implicit arithmetic: MultiplicationArithmetic[C, Real, C]): MaclaurinSeries = {
        new GeneratedMaclaurinSeries(x, generator)
    }

}

private class NegatedMaclaurinSeries[S <: MaclaurinSeries](override val series: S)
    extends NegatedPowerSeries(series) with MaclaurinSeries {

    override def of = series.of

    override def dx = -(series.dx)

    override def unary_-() = series

}

private class GeneratedMaclaurinSeries[C <: Constant]
(val x: Variable, val generator: PowerSeriesCoefficients[C])
(implicit arithmetic: MultiplicationArithmetic[C, Real, C])
    extends MaclaurinSeries {

    override type Coefficient = C

    override def isEmpty = generator.isEmpty

    override def of = x

    override def coefficient(power: Natural) = generator.value(power)

    override def dx = MaclaurinSeries(x, new TransformedPowerSeriesCoefficients[C, C](generator, (n, t) => arithmetic.multiply(t, n)))

}