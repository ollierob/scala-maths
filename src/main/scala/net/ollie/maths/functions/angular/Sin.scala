package net.ollie.maths.functions.angular

import net.ollie.maths._
import net.ollie.maths.functions.{CompositeBuilder, ExpressionBuilder, UnivariateFunction}
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers.{Precision, RealNumber, Zero}

/**
 * Created by Ollie on 02/01/14.
 */
object Sin
        extends UnivariateFunction[Angle, RealNumber]
        with ExpressionBuilder {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: RealNumber => Sin(Radians(re))
        case _ => ???
    }

    def apply(angle: Angle) = if (angle.isEmpty) empty else new RealSin(angle)

    protected[this] def create(expr: Expression): Expression = new Sin(expr)

    protected[angular] def empty = Zero

}

private class Sin(val of: Expression)
        extends CompositeBuilder {

    protected[this] def builder = Sin

    def isEmpty = of.isEmpty

    protected[this] def derivative(at: Expression) = Cos(at)

    override def toString = s"Sin($of)"

}

/**
 * TODO periodicity
 * @param of
 */
private class RealSin(override val of: Angle)
        extends Sin(of)
        with RealNumber {

    private lazy val series = MaclaurinSeries(Sin, of.toRadians)

    protected[this] def eval(precision: Precision) = series.evaluate(precision)

    override def variables = super[RealNumber].variables

    override def toConstant = Some(this)

}

object Cosec
        extends ExpressionBuilder {

    def apply(n: Number) = Sin(n).inverse

    protected[this] def create(expr: Expression) = 1 / Sin(expr)

    protected[this] def empty = Sin.empty.inverse
}