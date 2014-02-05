package net.ollie.maths.functions.numeric

import net.ollie.maths.Number
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.Complex

/**
 * Created by Ollie on 05/01/14.
 */
object Signum
        extends UnivariateFunction[Real, Integer] {

    def apply(n: Number): Integer = n match {
        case Zero => Zero
        case re: Real => apply(re)
        case z: Complex => apply(z)
        case _ => ???
    }

    def apply(re: Real): Integer = re match {
        case Zero => Zero
        case _ if re.isStrictlyPositive => One
        case _ => MinusOne
    }

    def apply(z: Complex): Integer = ComplexSignum(z)

    override def toString = "sgn(?)"

}

/**
 * The signum of the real part, or if is zero, the real multiplier of the imaginary part.
 */
object ComplexSignum
        extends UnivariateFunction[Complex, Integer] {

    def apply(z: Complex): Integer = Signum(z.re) match {
        case Zero => Signum(z.im)
        case otherwise => otherwise
    }

    override def toString = "csgn(?)"

}
