package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.expressions.Univariate

trait MaclaurinSeries
    extends PowerSeries with Univariate {

    override def of: Variable

    override def variable = of

    override def variables = Set(of)

}