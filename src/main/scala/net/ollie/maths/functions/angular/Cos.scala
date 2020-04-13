package net.ollie.maths.functions.angular

import ch.obermuhlner.math.big.BigDecimalMath
import net.ollie.maths._
import net.ollie.maths.expressions.{Expression, Invertible}
import net.ollie.maths.functions.{BuiltFunction, FunctionBuilder, RealFunctionBuilder}
import net.ollie.maths.numbers.constants.{MinusOne, One, Pi, Zero}
import net.ollie.maths.numbers.{Integer, PositiveReal, Precision, Real}
import net.ollie.utils.BigDecimals

/**
 * Created by Ollie on 03/01/14.
 */
object Cos
    extends RealFunctionBuilder {

    import Angle._

    def apply(re: Real): Real with Cos = re match {
        case Zero => CosZero
        case Pi => new CosPi(1) //TODO other multiples of Pi
        case a: Angle => apply(a)
        case _ => apply(re radians)
    }

    def apply(angle: Angle): Real with Cos = angle match {
        case PiRadians(n) => new CosPi(n)
        case _ => new RealCos(angle)
    }

    def unapply(cos: Cos): Option[Expression] = Some(cos.of)

    protected[this] def create(expr: Expression) = new CosOf(expr)

    override protected[angular] def empty = One

}

trait Cos
    extends Expression {

    val of: Expression

    override def toString = s"Cos($of)"

}

class CosOf(val of: Expression)
    extends BuiltFunction
        with Cos
        with Invertible {

    protected[this] def builder = Cos

    protected[this] def derivative(x: Expression) = -Sin(x)

    def inverse = ArcCos(of)

    def isEmpty = false

    override def equals(that: Expression) = that match {
        case Cos(y) => of == y || super.equals(that)
        case _ => super.equals(that)
    }

    override def toString = s"Cos($of)"

}

private class CosPi(val n: Integer)
    extends PositiveReal with Cos {

    override val of = n * Pi

    override def isEmpty = false

    override def abs = One

    override def evaluate(precision: Precision) = {
        if (n.isEven) One.BIG_DECIMAL to precision
        else MinusOne.BIG_DECIMAL to precision
    }

    override def toString = s"$n * $Pi"

}

private object CosZero
    extends CosPi(0)

private class RealCos(override val of: Angle)
    extends Real
        with Cos
        with CachedEvaluated {

    protected[this] def doEvaluate(precision: Precision) = {
        BigDecimals.cos(of.evaluate(precision), precision)
    }

    private lazy val empty: Boolean = {
        (2 * of / Pi) match {
            case i: Integer => !i.isEven
            case _ => false
        }
    }

    def isEmpty = empty

    override def inverse = super[Real].inverse

    override def variables = super[Real].variables

    override def toConstant = Some(this)

}

object Sec
    extends FunctionBuilder {

    def apply(n: Constant) = Cos(n).inverse

    def apply(re: Real): Real = Cos(re).inverse

    def apply(re: Angle): Real = Cos(re).inverse

    protected[this] def create(expr: Expression) = 1 / Cos(expr)

    override protected[this] def empty = Cos.empty.inverse

}
