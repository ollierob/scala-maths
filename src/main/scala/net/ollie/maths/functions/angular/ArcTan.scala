package net.ollie.maths.functions.angular

import net.ollie.maths.functions.{BuiltFunction, UnivariateFunction, RealFunctionBuilder}
import net.ollie.maths.Expression
import net.ollie.maths.numbers.{Precision, Natural, Real}
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.constants.MinusOne
import org.nevec.rjm.BigDecimalMath

/**
 *
 * Created by Ollie on 09/02/14.
 * @see http://mathworld.wolfram.com/InverseTangent.html
 */
object ArcTan
        extends RealFunctionBuilder
        with UnivariateFunction[Real, Angle] {

    def apply(re: Real): Angle with ArcTan = new RealArcTan(re)

    protected[this] def create(expr: Expression) = new ExpressionArcTan(expr)

    protected[this] def empty: Angle = EmptyAngle

}

trait ArcTan
        extends Expression {

    val of: Expression

    override def toString = s"ArcTan($of)"

}

class ExpressionArcTan(val of: Expression)
        extends BuiltFunction
        with ArcTan {

    protected[this] def builder = ArcTan

    def isEmpty = of.isEmpty

    protected[this] def derivative(x: Expression) = 1 / (1 + (x ^ 2))

}

class RealArcTan(val of: Real)
        extends Radians
        with ArcTan {

    val value: Real = new ArcTanEvaluator

    override def isEmpty = of.isEmpty

    private class ArcTanEvaluator
            extends Real {

        def isEmpty = RealArcTan.this.isEmpty

        def evaluate(precision: Precision) = {
            BigDecimalMath.atan(of.evaluate(precision).underlying())
        }

        override def toString = RealArcTan.this.toString

    }

}