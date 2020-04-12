package net.ollie.maths.functions.polynomial

import net.ollie.maths.Constant
import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.numbers.Natural

trait PowerSeries
    extends Expression {

    def of: Expression

    def coefficient(power: Natural): Constant

}

trait UnivariateCenteredPowerSeries
    extends PowerSeries with Univariate {

    def center: Constant

    override def of = variable - center

}