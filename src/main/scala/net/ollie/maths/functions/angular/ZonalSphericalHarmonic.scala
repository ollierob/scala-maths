package net.ollie.maths.functions.angular

import net.ollie.maths.{Differentiable, Variable}
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

    final override def m = Zero

    override def toString = "Z(" + l + ")"

}

object ZeroZeroHarmonic
        extends ZonalSphericalHarmonic {

    def l = Zero

    private final val value: RealNumber = PositiveSquareRoot(One / Pi) / 2

    protected[this] def f = value

}

class LZeroHarmonic(val l: NaturalNumber, val theta: Variable)
        extends AnyRef
        with ZonalSphericalHarmonic {

    private final def func: Differentiable = PositiveSquareRoot(((2 * l + 1) !) / (Pi * NaturalNumber(4))) * LegendrePolynomial(l, Cos(theta))

    protected[this] def f = func

}