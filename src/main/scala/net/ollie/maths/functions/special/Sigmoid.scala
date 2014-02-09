package net.ollie.maths.functions.special

import net.ollie.maths.{Expression, Number}
import net.ollie.maths.functions.{FunctionBuilder, UnivariateFunction}
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Half

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/SigmoidFunction.html
 */
object Sigmoid
        extends FunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(n: Number): Number = n match {
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(re: Real): Real = 1 / (1 + Exp(-re))

    protected[this] def create(expr: Expression) = 1 / (1 + Exp(-expr))

    protected[this] def empty = Half

}
