package net.ollie.maths.functions.angular

import scala.Some

import net.ollie.maths._
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder, FunctionBuilder}
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 03/01/14.
 */
object Cos
        extends RealFunctionBuilder {

    import Angle._

    def apply(re: Real): Real with Cos = re match {
        case a: Angle => apply(a)
        case _ => apply(re radians)
    }

    def apply(angle: Angle): Real with Cos = new RealCos(angle)

    protected[this] def create(expr: Expression): Cos = new CosOf(expr)

    protected[angular] def empty = One

}

trait Cos extends Expression {

    val of: Expression

    override def toString = s"Cos($of)"

}

class CosOf(val of: Expression)
        extends BuiltFunction
        with Cos
        with Invertible {

    type Inverse = Expression //TODO

    protected[this] def builder = Cos

    protected[this] def derivative(x: Expression) = -Sin(x)

    def inverse = ArcCos(of)

    def isEmpty = false

    override def toString = s"Cos($of)"

}

class RealCos(override val of: Angle)
        extends Real
        with Cos {

    private lazy val series = MaclaurinSeries(Cos, of.toRadians)

    protected[this] def doEvaluate(precision: Precision) = series.evaluate(precision)

    def isEmpty = ??? //TODO mod

    override def inverse = super[Real].inverse

    override def variables = super[Real].variables

    override def toConstant = Some(this)

}

object Sec
        extends FunctionBuilder {

    def apply(n: Number) = Cos(n).inverse

    def apply(re: Angle): Real = Cos(re).inverse

    protected[this] def create(expr: Expression) = 1 / Cos(expr)

    protected[this] def empty = Cos.empty.inverse

}
