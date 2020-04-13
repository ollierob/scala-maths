package net.ollie.maths.functions.angular

import ch.obermuhlner.math.big.BigDecimalMath
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{EmptyConstant, Precision, Real}

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
        if (re.isZero) ZeroArcTan
        //TODO arctan(1) = pi/4 rad
        else new RealArcTan(re)
    }

    protected[this] def create(expr: Expression) = new ExpressionArcTan(expr)

    protected[this] def empty: Angle = EmptyAngle

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

private object ZeroArcTan
    extends ArcTan with Radians with EmptyConstant {

    override val of = Zero

    override val value = Zero

    override def unary_-() = this

    override def abs = Zero

    override def evaluate(precision: Precision) = Zero.BIG_DECIMAL

}

private class RealArcTan(val of: Real)
    extends ArcTan with Radians {

    val value: Real = new ArcTanEvaluator

    override def isEmpty = of.isEmpty //Only zero at x=0

    override def toString = super[ArcTan].toString

    private class ArcTanEvaluator
        extends Real {

        def isEmpty = RealArcTan.this.isEmpty

        var maxPrecision: Precision = _
        var evaluated: BigDecimal = _

        def evaluate(precision: Precision) = {
            if (maxPrecision == null || !(precision > maxPrecision).contains(false)) {
                maxPrecision = precision
                evaluated = BigDecimalMath.atan(of.evaluate(precision).underlying(), precision.toMathContext)
            }
            evaluated
        }

        override def toString = RealArcTan.this.toString

    }

}