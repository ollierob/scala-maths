package net.ollie.maths.functions.numeric

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{BuiltFunction, ComplexFunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{MinusOne, One, Zero}

/**
 * Created by Ollie on 05/01/14.
 */
object Signum
        extends ComplexFunctionBuilder
        with UnivariateFunction[Real, Integer] {

    type Z = Integer

    override def apply(re: Real): Integer = re match {
        case Zero => Zero
        case _ if re.isStrictlyPositive => One
        case _ => MinusOne
    }

    /**
     * The signum of the real part, or if is zero, the real multiplier of the imaginary part.
     */
    def apply(z: Complex): Integer = Signum(z.re) match {
        case Zero => Signum(z.im)
        case otherwise => otherwise
    }

    protected[this] def empty = Zero

    protected[this] def create(expr: Expression) = new SignumOf(expr)

    override def toString = "Signum"

}

class SignumOf(val of: Expression)
        extends BuiltFunction {

    def isEmpty = of.isEmpty

    protected[this] def derivative(x: Expression) = 2 * DiracDelta(x)

    protected[this] def builder = Signum

    override def toString = s"Signum($of)"

}