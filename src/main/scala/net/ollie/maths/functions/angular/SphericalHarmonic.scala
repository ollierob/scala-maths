package net.ollie.maths.functions.angular

import net.ollie.maths._
import net.ollie.maths.functions.{Modal, Represented}
import net.ollie.maths.functions.numeric.{Exp, PositiveSquareRoot}
import net.ollie.maths.functions.polynomial.AssociatedLegendrePolynomial
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{ImaginaryUnit => i}
import net.ollie.maths.numbers.constants.{MinusOne, Zero, Pi}

/**
 * Created by Ollie on 08/01/14.
 */
trait SphericalHarmonic
        extends Expression
        with Modal {

    require(l >= m.abs)

    /**
     * The degree.
     * @return
     */
    def l: Natural

    def degree = l

    /**
     * The order. Should satisfy |m| <= l.
     * @return
     */
    def m: Integer

    def minusM: SphericalHarmonic

    def order = m

    def theta: Expression

    def phi: Expression

    def conjugate: SphericalHarmonic = SphericalHarmonic.conjugate(this)

    override def toString = s"Y($l,$m)"

}

object SphericalHarmonic {

    def apply(l: Natural, m: Integer, theta: Expression, phi: Expression): SphericalHarmonic = (l, m) match {
        case (_, Zero) => apply(l, theta)
        case _ if m.abs > l => new EmptyHarmonic(l, m, theta, phi)
        case _ => new LMHarmonic(l, m, theta, phi)
    }

    def apply(l: Natural, theta: Expression): ZonalSphericalHarmonic = ZonalSphericalHarmonic(l, theta)

    def conjugate(ylm: SphericalHarmonic): SphericalHarmonic = ylm match {
        case conjugated: ConjugatedSphericalHarmonic => conjugated.ylm
        case _ => new ConjugatedSphericalHarmonic(ylm)
    }

}

private class EmptyHarmonic(val l: Natural, val m: Integer, val theta: Expression, val phi: Expression)
        extends SphericalHarmonic
        with Empty {

    def minusM = SphericalHarmonic(l, -m, theta, phi)

    override def df(x: Variable) = Zero

    def replace(variables: Map[Variable, Expression]) = SphericalHarmonic(l, m, theta.replace(variables), phi.replace(variables))

}

class LMHarmonic(val l: Natural, val m: Integer, val theta: Expression, val phi: Expression)
        extends SphericalHarmonic
        with Represented {

    require(l >= m.abs)

    def lmm: Natural = l - m

    def lpm: Natural = l + m

    def representation = (PositiveSquareRoot(((2 * l) + 1) * (lmm !) / (4 * Pi * (lpm !)))
            * AssociatedLegendrePolynomial(l, m, Cos(theta))
            * Exp(i * m * phi))

    def minusM = SphericalHarmonic(l, -m, theta, phi)

}

class ConjugatedSphericalHarmonic(val ylm: SphericalHarmonic)
        extends SphericalHarmonic
        with Represented {

    def l = ylm.l

    def m = -(ylm.m)

    override def conjugate = ylm

    def representation = (MinusOne ^ m) * ylm.minusM

    def theta = ylm.theta

    def phi = ylm.phi

    def minusM = ylm.minusM.conjugate

    override def toString = s"($ylm)*"

}
