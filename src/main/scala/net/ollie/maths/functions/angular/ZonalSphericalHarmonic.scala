package net.ollie.maths.functions.angular

import net.ollie.maths.{Expression, Variable}
import net.ollie.maths.functions.Represented
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.functions.polynomial.LegendrePolynomial
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.real.Pi

/**
 * Order-less spherical harmonics.
 * Created by Ollie on 09/01/14.
 */
trait ZonalSphericalHarmonic
        extends SphericalHarmonic {

    override final def m = Zero

    override def toString = "Z(" + l + ")"

}

object ZeroZeroHarmonic
        extends ZonalSphericalHarmonic
        with Represented {

    def l = Zero

    private final val value: RealNumber = PositiveSquareRoot(One / Pi) / 2

    protected[this] def f = value

}

class LZeroHarmonic(val l: NaturalNumber, val theta: Variable)
        extends AnyRef
        with ZonalSphericalHarmonic
        with Represented {

    private val func: Expression = PositiveSquareRoot(((2 * l + 1) !) / (Pi * NaturalNumber(4))) * LegendrePolynomial(l, Cos(theta))

    protected[this] def f = func

}