package net.ollie.maths.functions.angular

import ch.obermuhlner.math.big.BigDecimalMath
import net.ollie.maths._
import net.ollie.maths.expressions.{Expression, Invertible}
import net.ollie.maths.functions.angular.Angle._
import net.ollie.maths.functions.special.Sinc
import net.ollie.maths.functions.{FunctionBuilder, OddBuiltFunction, RealFunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Pi, Zero}

/**
 * Created by Ollie on 02/01/14.
 */
object Sin
    extends RealFunctionBuilder
        with UnivariateFunction[Angle, Real] {

    def apply(re: Real): Real with Sin = re match {
        case Zero => SinPi
        case Pi => SinPi //TODO other even & odd multiples
        case angle: Angle => apply(angle)
        case _ => apply(re radians)
    }

    def apply(angle: Angle): Real with Sin = {
        if (angle.isEmpty) empty
        else new RealSin(angle)
    }

    def unapply(sin: Sin): Option[Expression] = Some(sin.of)

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

    protected[this] def builder = Sin

    def isEmpty = of.isEmpty

    protected[this] def derivative(at: Expression) = Cos(at)

    def inverse = ArcSin(of)

    override def ?/(that: Expression) = {
        if (of equals that) Some(Sinc(of))
        else super.?/(that)
    }

    override def toString = s"Sin($of)"

}

private object SinPi
    extends Real with Sin with EmptyConstant {

    override val of = Pi

    override def abs = Zero

}

/**
 * TODO periodicity
 *
 * @param of
 */
class RealSin(val of: Angle)
    extends Real
        with Sin
        with CachedEvaluated {

    private lazy val reduced: Angle = of.reduce

    protected[this] def doEvaluate(precision: Precision) = {
        //FIXME BigDecimalMath.sin is crap and blows up too often e.g. sin(3.3) => explode
        BigDecimalMath.sin(reduced.evaluate(precision).underlying(), precision.toMathContext)
    }

    override def variables = super[Real].variables

    override def toConstant = Some(this)

    private lazy val empty: Boolean = {
        (reduced / Pi).toRadians.value match {
            case i: Integer => true
            case _ => false
        }
    }

    val isEmpty = empty

    override def toString = s"Sin($of)"

}

private object SinZero
    extends Real
        with Sin
        with EmptyConstant {

    val of = Zero

    override def abs = super[EmptyConstant].abs

}

object Cosec
    extends FunctionBuilder {

    def apply(n: Constant) = Sin(n).inverse

    protected[this] def create(expr: Expression) = 1 / Sin(expr)

    protected[this] def empty = Sin.empty.inverse

}
