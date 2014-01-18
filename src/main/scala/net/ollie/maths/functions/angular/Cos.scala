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
        case ang: Angle => apply(ang.toRadians)
        case re: RealNumber => apply(re)
        case _ => ???
    }

    def apply(re: RealNumber): RealNumber = if (re.isEmpty) One else new RealCos(re)

    protected[this] def create(expr: Expression): Expression = new Cos(expr)

    protected[this] def create(diff: Differentiable): Differentiable = new DifferentiableCos(diff)

    protected[this] def empty = One

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

class RealCos(override val of: RealNumber)
        extends DifferentiableCos(of)
        with RealNumber {

    private lazy val series = MaclaurinSeries(Cos, of)

    protected[this] def eval(precision: Precision) = series.evaluate(precision)

    override def variables = super[RealNumber].variables

    override def toConstant = Some(this)

}
