package net.ollie.maths.functions.angular

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{EmptyConstant, Precision, Rational, Real}
import net.ollie.utils.{BigDecimals, ValueCache}

/**
 *
 * Created by Ollie on 09/02/14.
 *
 * @see http://mathworld.wolfram.com/InverseTangent.html
 */
object ArcTan
    extends RealFunctionBuilder
        with UnivariateFunction[Real, Angle] {

    def apply(re: Real): Angle with ArcTan = {
        if (re.isZero) ZeroArcTanRadians
        //TODO arctan(1) = pi/4 rad
        else new RealArcTanRadians(re)
    }

    protected[this] def create(expr: Expression) = new ExpressionArcTan(expr)

    override protected[this] def empty: Angle = EmptyAngle

    private[angular] def createReal(re: Real): RealArcTan = RealArcTanCache(re)

    private object RealArcTanCache
        extends ValueCache[Real, RealArcTan] {

        override protected[this] def compute(key: Real) = new RealArcTan(key)

        override protected[this] def shouldCache(k: Real) = k match {
            case _: Rational => true
            case _ => false
        }

    }

}

trait ArcTan
    extends Expression {

    val of: Expression

    override def toString = s"ArcTan($of)"

}

class ExpressionArcTan(val of: Expression)
    extends ArcTan with BuiltFunction {

    protected[this] def builder = ArcTan

    def isEmpty = of.isEmpty

    protected[this] def derivative(x: Expression) = 1 / (1 + (x ^ 2))

}

private object ZeroArcTanRadians
    extends ArcTan with Radians with EmptyConstant {

    override val of = Zero

    override val value = Zero

    override def unary_-() = this

    override def abs = Zero

    override def evaluate(precision: Precision) = Zero.BIG_DECIMAL

}

private class RealArcTanRadians(val of: Real)
    extends ArcTan with Radians {

    override lazy val value: Real = ArcTan.createReal(of)

    override def isEmpty = of.isEmpty //Only zero at x=0

    override def toString = super[ArcTan].toString

}

private class RealArcTan(val of: Real)
    extends ArcTan with Real with CachedEvaluated {

    def isEmpty = of.isEmpty

    override def toString = super[ArcTan].toString

    override protected[this] def doEvaluate(precision: Precision): BigDecimal = {
        BigDecimals.atan(of.evaluate(precision), precision)
    }

}