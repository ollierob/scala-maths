package net.ollie.maths.functions.angular

import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.{Expression, Number}
import net.ollie.maths.functions.{CompositeBuildable, ExpressionBuilder, UnivariateFunction}
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
        extends CompositeBuildable {

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

    override def approx(precision: Precision)(implicit mode: RoundingMode.RoundingMode) = f.approximatelyEvaluate(precision)

    override def toConstant = Some(this)

    override def variables = super[RealNumber].variables

}

object Cotan
        extends ExpressionBuilder {

    def apply(n: Number) = Tan(n).inverse

    protected[this] def create(expr: Expression) = 1 / Tan(expr)

    protected[this] def empty = Tan.empty.inverse
}