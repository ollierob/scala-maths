package net.ollie.maths.functions.angular

import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.{Differentiable, Expression, Number, Variable}
import net.ollie.maths.functions.{CompositeBuildable, DifferentiableExpressionBuilder, UnivariateFunction}
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.{Precision, RealNumber, Zero}

/**
 * Created by Ollie on 18/01/14.
 */
object Tan
        extends DifferentiableExpressionBuilder
        with UnivariateFunction[RealNumber, RealNumber] {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: RealNumber => apply(re)
        case _ => ???
    }

    def apply(f: RealNumber) = ???

    protected[this] def create(expr: Expression): Expression = ???

    protected[this] def create(diff: Differentiable): Differentiable = ???

    protected[angular] def empty = Zero

}

class Tan(val of: Expression)
        extends CompositeBuildable {

    protected[this] def builder = Tan

    def isEmpty = of.isEmpty

    override def toString = s"Tan($of)"

}

class DifferentiableTan(override val of: Differentiable)
        extends Tan(of)
        with Differentiable {

    def df(x: Variable) = Sec(x) ^ 2

}

class RealTan(override val of: RealNumber)
        extends Tan(of)
        with RealNumber
        with ApproximatelyEvaluated {

    private lazy val f = Sin(of) / Cos(of)

    override def approx(precision: Precision)(implicit mode: RoundingMode.RoundingMode) = f.approximatelyEvaluate(precision)

    override def toConstant = Some(this)

    override def variables = super[RealNumber].variables

}

object Cotan
        extends DifferentiableExpressionBuilder {

    def apply(n: Number) = Tan(n).inverse

    protected[this] def create(expr: Expression) = 1 / Tan(expr)

    protected[this] def create(diff: Differentiable) = 1 / Tan(diff)

    protected[this] def empty = Tan.empty.inverse
}