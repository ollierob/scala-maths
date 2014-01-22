package net.ollie.maths.functions.numeric

import net.ollie.maths.Number
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.ComplexNumber

/**
 * Created by Ollie on 05/01/14.
 */
object Signum
        extends UnivariateFunction[RealNumber, IntegerNumber] {

    def apply(n: Number): IntegerNumber = n match {
        case Zero => Zero
        case re: RealNumber => apply(re)
        case z: ComplexNumber => apply(z)
        case _ => ???
    }

    def apply(re: RealNumber): IntegerNumber = re match {
        case Zero => Zero
        case _ if re.isStrictlyPositive => One
        case _ => MinusOne
    }

    def apply(z: ComplexNumber): IntegerNumber = ComplexSignum(z)

    override def toString = "sgn(?)"

}

/**
 * The signum of the real part, or if is zero, the real multiplier of the imaginary part.
 */
object ComplexSignum
        extends UnivariateFunction[ComplexNumber, IntegerNumber] {

    def apply(z: ComplexNumber): IntegerNumber = Signum(z.re) match {
        case Zero => Signum(z.im.coefficient)
        case otherwise => otherwise
    }

    override def toString = "csgn(?)"

}
