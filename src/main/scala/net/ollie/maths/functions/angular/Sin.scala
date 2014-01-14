package net.ollie.maths.functions.angular

import scala.math.BigDecimal.RoundingMode

import net.ollie.maths._
import net.ollie.maths.functions.{DifferentiableUnivariateExpressionBuilder, UnivariateFunction}
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers.{Precision, RealNumber, Zero}

/**
 * Created by Ollie on 02/01/14.
 */
object Sin
        extends UnivariateFunction[RealNumber, RealNumber]
        with DifferentiableUnivariateExpressionBuilder {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: RealNumber => Sin(re)
        case _ => ???
    }

    def apply(re: RealNumber): RealNumber = new RealSin(re)

    def apply(x: Variable): DifferentiableUnivariate = new UnivariateSin(x)

    protected[this] def create(expr: Expression): Expression = new Sin(expr)

    protected[this] def create(diff: Differentiable): Differentiable = new DifferentiableSin(diff)

    protected[this] def empty = Zero

}

private class Sin(val of: Expression)
        extends Composite {

    protected[this] def at(n: Number) = Sin(n)

    protected[this] def apply(expr: Expression) = Sin(expr)

    def isEmpty = of.isEmpty

    override def toString = s"Sin($of)"

}

private class DifferentiableSin(override val of: Differentiable)
        extends Sin(of)
        with DifferentiableComposite {

    protected[this] def df(expression: Differentiable) = Cos(expression)

}

private class UnivariateSin(val variable: Variable)
        extends DifferentiableSin(variable)
        with DifferentiableUnivariate {

    override def variables = super[DifferentiableUnivariate].variables

    override def df(x: Variable) = Cos(x)

}

/**
 * TODO periodicity
 * @param of
 */
private class RealSin(override val of: RealNumber)
        extends Sin(of)
        with RealNumber {

    private lazy val series = MaclaurinSeries(Sin, of)

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode.RoundingMode) = series.evaluate(precision)

    override def variables = super[RealNumber].variables

    override def toConstant = Some(this)

}
