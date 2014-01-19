package net.ollie.maths.functions.angular


import net.ollie.maths._
import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers.{One, Precision, RealNumber}

/**
 * Created by Ollie on 03/01/14.
 */
object Cos
        extends ExpressionBuilder {

    def apply(n: Number): Number = n match {
        case re: RealNumber => apply(Radians(re))
        case _ => ???
    }

    def apply(angle: Angle) = if (angle.isEmpty) empty else new RealCos(angle)

    protected[this] def create(expr: Expression): Expression = new Cos(expr)

    protected[angular] def empty = One

}

class Cos(val of: Expression)
        extends Composite {

    protected[this] def at(n: Number) = Cos(n)

    protected[this] def apply(expr: Expression) = Cos(expr)

    def isEmpty = false //TODO

    protected[this] def derivative(at: Expression) = -Sin(at)

    override def toString = s"Cos($of)"

}

class RealCos(override val of: Angle)
        extends Cos(of)
        with RealNumber {

    private lazy val series = MaclaurinSeries(Cos, of.toRadians)

    protected[this] def eval(precision: Precision) = series.evaluate(precision)

    override def variables = super[RealNumber].variables

    override def toConstant = Some(this)

}

object Sec
        extends ExpressionBuilder {

    def apply(n: Number) = Cos(n).inverse

    protected[this] def create(expr: Expression) = 1 / Cos(expr)

    protected[this] def empty = Cos.empty.inverse

}
