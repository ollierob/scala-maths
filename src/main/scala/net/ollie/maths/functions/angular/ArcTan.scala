package net.ollie.maths.functions.angular

import net.ollie.maths.functions.{BuiltFunction, UnivariateFunction, RealFunctionBuilder}
import net.ollie.maths.Expression
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.constants.{MinusOne, Zero}

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

    private lazy val series: Real = Series(nth _, Zero)

    private def nth(n: Natural): Real = {
        val t = (2 * n) + 1
        (MinusOne ^ n) * (of ^ t) / t
    }

    def value = series

    override def isEmpty = of.isEmpty

}