package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.functions._
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.Expression
import net.ollie.maths.numbers.constants.One
import net.ollie.maths.functions.numeric.{Abs, Ln}
import net.ollie.maths.methods.Series

/**
 * Created by Ollie on 02/03/14.
 * @see http://mathworld.wolfram.com/ExponentialIntegral.html
 */
object ExponentialIntegral
        extends RealFunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(f: Real): Real = ???

    protected[this] def empty: Expression = ???

    protected[this] def create(expr: Expression): Expression = ???

}

trait ExponentialIntegral
        extends Expression {

    def of: Expression

    override def toString = s"ExponentialIntegral($of)"

}

class ExponentialIntegralOf(val of: Expression)
        extends ExponentialIntegral
        with BuiltFunction {

    require(!of.isEmpty)

    override protected[this] def derivative(x: Expression): Expression = ???

    override protected[this] def builder: FunctionBuilder = ???

    //    override def representation = {
    //        EulerMascheroniConstant + Ln(Abs(of)) + Series(nth _, One)
    //    }

    private def nth(n: Natural): Expression = {
        (of ^ n) / (n * n !)
    }

    def isEmpty = false //TODO

}