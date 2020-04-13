package net.ollie.maths.functions.numeric

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{BuiltFunction, ComplexFunctionBuilder, FunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 26/02/14.
 */
object Abs
        extends ComplexFunctionBuilder
        with UnivariateFunction[Complex, Real] {

    type Z = Real

    def apply(f: Complex) = f.abs

    protected[this] def create(expr: Expression) = new AbsOf(expr)

}

class AbsOf(val of: Expression)
        extends BuiltFunction {

    def isEmpty = false

    protected[this] def derivative(x: Expression) = Signum(x)

    protected[this] def builder = Abs

    override def toString = s"|$of|"

}