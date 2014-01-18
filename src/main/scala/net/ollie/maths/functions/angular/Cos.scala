package net.ollie.maths.functions.angular


import net.ollie.maths._
import net.ollie.maths.functions.DifferentiableExpressionBuilder
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers.{One, Precision, RealNumber}

/**
 * Created by Ollie on 03/01/14.
 */
object Cos
        extends DifferentiableExpressionBuilder {

    def apply(n: Number): Number = n match {
        case re: RealNumber => apply(Radians(re))
        case _ => ???
    }

    def apply(angle: Angle) = if (angle.isEmpty) empty else new RealCos(angle)

    protected[this] def create(expr: Expression): Expression = new Cos(expr)

    protected[this] def create(diff: Differentiable): Differentiable = new DifferentiableCos(diff)

    protected[angular] def empty = One

}

class Cos(val of: Expression)
        extends Composite {

    protected[this] def at(n: Number) = Cos(n)

    protected[this] def apply(expr: Expression) = Cos(expr)

    def isEmpty = false //TODO

    override def toString = s"Cos($of)"

}

class DifferentiableCos(override val of: Differentiable)
        extends Cos(of)
        with DifferentiableComposite {

    protected[this] def df(of: Differentiable) = -Sin(of)

}

class RealCos(override val of: Angle)
        extends DifferentiableCos(of)
        with RealNumber {

    private lazy val series = MaclaurinSeries(Cos, of.toRadians)

    protected[this] def eval(precision: Precision) = series.evaluate(precision)

    override def variables = super[RealNumber].variables

    override def toConstant = Some(this)

}

object Sec
        extends DifferentiableExpressionBuilder {

    def apply(n: Number) = Cos(n).inverse

    protected[this] def create(expr: Expression) = 1 / Cos(expr)

    protected[this] def create(diff: Differentiable) = 1 / Cos(diff)

    protected[this] def empty = Cos.empty.inverse

}
