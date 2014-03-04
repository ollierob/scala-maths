package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.functions.{BuiltFunction, UnivariateFunction, RealFunctionBuilder}
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.{CachedEvaluated, Expression}
import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.numbers.constants.{Zero, PositiveNamedReal}

/**
 * Created by Ollie on 02/03/14.
 * @see http://mathworld.wolfram.com/LogarithmicIntegral.html
 */
object LogarithmicIntegral
        extends RealFunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(f: Real): Real with LogarithmicIntegral = new RealLogarithmicIntegral(f)

    protected[this] def empty = Zero

    protected[this] def create(expr: Expression) = new LogarithmicIntegralOf(expr)

}

trait LogarithmicIntegral
        extends Expression {

    def of: Expression

    override def toString = s"LogarithmicIntegral($of)"

}

class LogarithmicIntegralOf(val of: Expression)
        extends LogarithmicIntegral
        with BuiltFunction {

    def isEmpty = of.isEmpty

    protected[this] def derivative(x: Expression) = 1 / Ln(x)

    protected[this] def builder = LogarithmicIntegral

}

class RealLogarithmicIntegral(val of: Real)
        extends LogarithmicIntegral
        with Real
        with CachedEvaluated {

    def isEmpty = of.isEmpty || SoldnersConstant == of

    override protected[this] def doEvaluate(precision: Precision) = {
        ???
    }

}

object SoldnersConstant
        extends PositiveNamedReal
        with CachedEvaluated {

    override protected[this] def doEvaluate(precision: Precision) = {
        ???
    }

}