package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Constant, Variable}
import net.ollie.maths.expressions.Univariate
import net.ollie.maths.numbers.Natural

trait UnivariatePowerSeries
    extends PowerSeries with Univariate {

    override def of: Variable

    override def variable = of

    override def variables = Set(of)

    def coefficient(power: Natural): Constant

}
