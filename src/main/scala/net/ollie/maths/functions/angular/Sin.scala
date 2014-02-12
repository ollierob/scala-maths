package net.ollie.maths.functions.angular

import scala.Some

import Angle._
import net.ollie.maths._
import net.ollie.maths.functions.{OddBuiltFunction, RealFunctionBuilder, FunctionBuilder, UnivariateFunction}
import net.ollie.maths.functions.special.Sinc
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 02/01/14.
 */
object Sin
        extends RealFunctionBuilder
        with UnivariateFunction[Angle, Real] {

    def apply(re: Real): Real with Sin = re match {
        case angle: Angle => apply(angle)
        case _ => apply(re radians)
    }

    def apply(angle: Angle): Real with Sin = if (angle.isEmpty) empty else new RealSin(angle)

    protected[this] def create(expr: Expression): Sin = new SinOf(expr)

    protected[angular] def empty = SinZero

}

trait Sin
        extends Expression {

    val of: Expression

    override def toString = s"Sin($of)"

}

private class SinOf(val of: Expression)
        extends OddBuiltFunction
        with Sin
        with Invertible {

    type Inverse = Expression //TODO

    protected[this] def builder = Sin

    def isEmpty = of.isEmpty

    protected[this] def derivative(at: Expression) = Cos(at)

    def inverse = ArcSin(of)

    override def /(that: Expression) = if (that equals of) Sinc(of) else super./(that)

    override def toString = s"Sin($of)"

}

/**
 * TODO periodicity
 * @param of
 */
class RealSin(val of: Angle)
        extends Real
        with Sin {

    private lazy val series = MaclaurinSeries(Sin, of.toRadians)

    protected[this] def doEvaluate(precision: Precision) = series.evaluate(precision)

    override def variables = super[Real].variables

    override def toConstant = Some(this)

    def isEmpty = of.isEmpty

    override def toString = s"Sin($of)"

}

private object SinZero
        extends Real
        with Sin
        with EmptyNumber {

    val of = Zero

    override def abs = super[EmptyNumber].abs

}

object Cosec
        extends FunctionBuilder {

    def apply(n: Number) = Sin(n).inverse

    protected[this] def create(expr: Expression) = 1 / Sin(expr)

    protected[this] def empty = Sin.empty.inverse

}
