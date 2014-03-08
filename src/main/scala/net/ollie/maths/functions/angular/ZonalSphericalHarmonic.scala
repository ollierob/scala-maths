package net.ollie.maths.functions.angular

import net.ollie.maths.Expression
import net.ollie.maths.functions.Represented
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.functions.polynomial.LegendrePolynomial
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One, Pi}

/**
 * Order-less spherical harmonics.
 * Created by Ollie on 09/01/14.
 */
trait ZonalSphericalHarmonic
        extends SphericalHarmonic {

    override final def order = Zero

    final def phi = Zero

    final def minusM: ZonalSphericalHarmonic = this

    override final def conjugate: ZonalSphericalHarmonic = this

    override def toString = "Z(" + degree + ")"

}

object ZonalSphericalHarmonic {

    def apply(l: Natural, theta: Expression): ZonalSphericalHarmonic = l match {
        case Zero => ZeroZeroHarmonic
        case _ => new LZeroHarmonic(l, theta)
    }

}

object ZeroZeroHarmonic
        extends ZonalSphericalHarmonic
        with Represented {

    def theta = Zero

    def degree = Zero

    private final val value: Real = PositiveSquareRoot(One / Pi) / 2

    def representation = value

}

class LZeroHarmonic(val degree: Natural, val theta: Expression)
        extends AnyRef
        with ZonalSphericalHarmonic
        with Represented {

    private lazy val repr: Expression = {
        PositiveSquareRoot(((2 * degree + 1) !) / (Pi * Natural(4))) * LegendrePolynomial(degree)(Cos(theta))
    }

    def representation = repr

}