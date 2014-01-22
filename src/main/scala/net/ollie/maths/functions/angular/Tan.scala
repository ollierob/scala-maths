package net.ollie.maths.functions.angular

import net.ollie.maths.{Expression, Number}
import net.ollie.maths.functions.{CompositeBuilder, ExpressionBuilder, UnivariateFunction}
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.{Precision, RealNumber, Zero}

/**
 * Created by Ollie on 18/01/14.
 */
object Tan
        extends ExpressionBuilder
        with UnivariateFunction[Angle, RealNumber] {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: RealNumber => apply(Radians(re))
        case _ => ???
    }

    def apply(angle: Angle) = new RealTan(angle)

    protected[this] def create(expr: Expression) = new Tan(expr)

    protected[angular] def empty = Zero

}

class Tan(val of: Expression)
        extends CompositeBuilder {

    protected[this] def builder = Tan

    def isEmpty = of.isEmpty

    def derivative(x: Expression) = Sec(x) ^ 2

    override def toString = s"Tan($of)"

}

class RealTan(override val of: Angle)
        extends Tan(of)
        with RealNumber
        with ApproximatelyEvaluated {

    private lazy val f: RealNumber = Sin(of) / Cos(of)

    override def approx(precision: Precision) = f.approximatelyEvaluate(precision)

    override def toConstant = Some(this)

    override def variables = super[RealNumber].variables

}

object Cotan
        extends ExpressionBuilder {

    def apply(n: Number) = Tan(n).inverse

    protected[this] def create(expr: Expression) = 1 / Tan(expr)

    protected[this] def empty = Tan.empty.inverse
}

object ArcTan
        extends ExpressionBuilder {

    def apply(n: Number): Number = ???

    def apply(re: RealNumber): Angle = ???

    protected[this] def create(expr: Expression): Expression = ???

    protected[this] def empty: Expression = ???

}