package net.ollie.maths.functions.angular

import net.ollie.maths.Constant
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{FunctionBuilder, OddBuiltFunction, RealFunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.methods.ApproximatelyEvaluated

/**
 * Created by Ollie on 18/01/14.
 */
object Tan
        extends RealFunctionBuilder
        with UnivariateFunction[Angle, Real] {

    import Angle._

    def apply(re: Real): Real = re match {
        case a: Angle => apply(a)
        case _ => apply(re radians)
    }

    def apply(angle: Angle): Real = if (angle.isEmpty) empty else new RealTan(angle)

    protected[this] def create(expr: Expression) = new Tan(expr)

    override protected[angular] def empty = Zero

}

class Tan(val of: Expression)
        extends OddBuiltFunction {

    protected[this] def builder = Tan

    def isEmpty = of.isEmpty

    def derivative(x: Expression) = Sec(x) ^ 2

    override def toString = s"Tan($of)"

}

class RealTan(override val of: Angle)
        extends Tan(of)
        with Real
        with ApproximatelyEvaluated {

    private lazy val f: Real = Sin(of) / Cos(of)

    protected[this] def doApproximatelyEvaluate(precision: Precision) = f.approximatelyEvaluate(precision)

    override def toConstant = Some(this)

    override def variables = super[Real].variables

}

/**
 * 1/Tan
 */
object CoTan
        extends FunctionBuilder {

    def apply(n: Constant): Constant = Tan(n).inverse

    def apply(re: Angle): Real = Tan(re).inverse

    protected[this] def create(expr: Expression) = 1 / Tan(expr)

    override protected[this] def empty = Tan.empty.inverse

}
