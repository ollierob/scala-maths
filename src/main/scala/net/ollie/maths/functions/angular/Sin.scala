package net.ollie.maths.functions.angular


import net.ollie.maths._
import net.ollie.maths.functions.{CompositeBuildable, DifferentiableExpressionBuilder, UnivariateFunction}
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers.{Precision, RealNumber, Zero}

/**
 * Created by Ollie on 02/01/14.
 */
object Sin
        extends UnivariateFunction[RealNumber, RealNumber]
        with DifferentiableExpressionBuilder {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: RealNumber => Sin(re)
        case _ => ???
    }

    def apply(re: RealNumber): RealNumber = new RealSin(re)

    protected[this] def create(expr: Expression): Expression = new Sin(expr)

    protected[this] def create(diff: Differentiable): Differentiable = new DifferentiableSin(diff)

    protected[angular] def empty = Zero

}

private class Sin(val of: Expression)
        extends CompositeBuildable {

    protected[this] def builder = Sin

    def isEmpty = of.isEmpty

    override def toString = s"Sin($of)"

}

private class DifferentiableSin(override val of: Differentiable)
        extends Sin(of)
        with DifferentiableComposite {

    protected[this] def df(expression: Differentiable) = Cos(expression)

}

/**
 * TODO periodicity
 * @param of
 */
private class RealSin(override val of: RealNumber)
        extends Sin(of)
        with RealNumber {

    private lazy val series = MaclaurinSeries(Sin, of)

    protected[this] def eval(precision: Precision) = series.evaluate(precision)

    override def variables = super[RealNumber].variables

    override def toConstant = Some(this)

}

object Cosec
        extends DifferentiableExpressionBuilder {

    def apply(n: Number) = Sin(n).inverse

    protected[this] def create(expr: Expression) = 1 / Sin(expr)

    protected[this] def create(diff: Differentiable) = 1 / Sin(diff)

    protected[this] def empty = Sin.empty.inverse
}