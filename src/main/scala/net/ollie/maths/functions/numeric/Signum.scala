package net.ollie.maths.functions.numeric

import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{MinusOne, Zero, One}

/**
 * Created by Ollie on 05/01/14.
 */
object Signum
        extends UnivariateFunction[Real, Integer] {

    def apply(re: Real): Integer = re match {
        case Zero => Zero
        case _ if re.isStrictlyPositive => One
        case _ => MinusOne
    }

    def apply(z: Complex): Integer = ComplexSignum(z)

    override def toString = "Signum(?)"

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

    override def toString = "ComplexSignum(?)"

}
