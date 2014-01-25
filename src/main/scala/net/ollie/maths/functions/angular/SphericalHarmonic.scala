package net.ollie.maths.functions.angular

import net.ollie.maths.{Expression, Variable}
import net.ollie.maths.functions.{Modal, Represented}
import net.ollie.maths.numbers._

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

    def order = m

    override def toString = s"Y($l,$m)"

}

object SphericalHarmonic {

    def apply(l: Natural, m: Integer, theta: Variable, phi: Variable): SphericalHarmonic = (l, m) match {
        case (_, Zero) => apply(l, theta)
        case _ => new LMHarmonic(l, m, theta, phi)
    }

    def apply(l: Natural, theta: Variable): ZonalSphericalHarmonic = l match {
        case Zero => ZeroZeroHarmonic
        case _ => new LZeroHarmonic(l, theta)
    }

}

class LMHarmonic(val l: Natural, val m: Integer, val theta: Variable, val phi: Variable)
        extends SphericalHarmonic
        with Represented {

    protected[this] def f = ???

}