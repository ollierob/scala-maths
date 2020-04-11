package net.ollie.maths.functions.special

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{RealFunctionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Half

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/SigmoidFunction.html
 */
object Sigmoid
        extends RealFunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(re: Real): Real = Real(apply(re.asInstanceOf[Expression]).toConstant).get

    protected[this] def create(expr: Expression) = new SigmoidOf(expr)

    protected[this] def empty = Half

}

trait Sigmoid
        extends Expression {

    def of: Expression

    override def toString = s"Sigmoid($of)"

}

class SigmoidOf(val of: Expression)
        extends Sigmoid
        with Represented {

    def representation = 1 / (1 + Exp(-of))

    override def isEmpty = false

}