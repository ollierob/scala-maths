package net.ollie.maths.functions.angular

import net.ollie.maths._
import net.ollie.maths.expressions.{Empty, Expression}
import net.ollie.maths.functions.numeric.{Exp, PositiveSquareRoot}
import net.ollie.maths.functions.polynomial.AssociatedLegendrePolynomial
import net.ollie.maths.functions.{Modal, Represented}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{ImaginaryUnit => i}
import net.ollie.maths.numbers.constants.{MinusOne, Pi, Zero}

/**
 * Created by Ollie on 08/01/14.
 */
trait SphericalHarmonic
    extends Expression
        with Modal {

    require(degree >= order.abs)

    def minusM: SphericalHarmonic

    def theta: Expression

    def phi: Expression

    def conjugate: SphericalHarmonic = SphericalHarmonic.conjugate(this)

    override def toString = s"Y($degree,$order)"

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

private class EmptyHarmonic(val degree: Natural, val order: Integer, val theta: Expression, val phi: Expression)
    extends SphericalHarmonic
        with Empty {

    def minusM = SphericalHarmonic(degree, -order, theta, phi)

    override def df(x: Variable) = Zero

    def replace(variables: Map[Variable, Expression]) = SphericalHarmonic(degree, order, theta.replace(variables), phi.replace(variables))

}

class LMHarmonic(val degree: Natural, val order: Integer, val theta: Expression, val phi: Expression)
    extends SphericalHarmonic
        with Represented {

    require(degree >= order.abs)

    lazy val lmm: Natural = degree - order

    lazy val lpm: Natural = degree + order

    def representation = (PositiveSquareRoot(((2 * degree) + 1) * (lmm !) / (4 * Pi * (lpm !)))
        * AssociatedLegendrePolynomial(degree, order)(Cos(theta))
        * Exp(i * order * phi))

    def minusM = SphericalHarmonic(degree, -order, theta, phi)

    override def isEmpty = super.isEmpty

}

class ConjugatedSphericalHarmonic(val ylm: SphericalHarmonic)
    extends SphericalHarmonic
        with Represented {

    def degree = ylm.degree

    def order = -(ylm.order)

    override def conjugate = ylm

    def representation = (MinusOne ^ order) * ylm.minusM

    def theta = ylm.theta

    def phi = ylm.phi

    def minusM = ylm.minusM.conjugate

    override def toString = s"($ylm)*"

}
