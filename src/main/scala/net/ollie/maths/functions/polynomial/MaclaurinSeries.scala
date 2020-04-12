package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.expressions.Univariate

trait MaclaurinSeries
    extends PowerSeries with Univariate {

    override def of: Variable

    override def variables = super[Univariate].variables

    override def variable = of

}

object MaclaurinSeries{

}