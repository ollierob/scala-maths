package net.ollie.maths.functions.angular

import net.ollie.maths.{Differentiable, Variable}
import net.ollie.maths.functions.{Modal, Represented}
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 08/01/14.
 */
trait SphericalHarmonic
        extends Modal
        with Differentiable
        with Represented {

    require(l >= m.abs)

    /**
     * The degree.
     * @return
     */
    def l: NaturalNumber

    def degree = l

    /**
     * The order. Should satisfy |m| <= l.
     * @return
     */
    def m: IntegerNumber

    def order = m

    protected[this] def f: Differentiable

    def df(x: Variable) = f.df(x)

    override def toString = "Y(" + l + "," + m + ")"

}

object SphericalHarmonic {

    def apply(l: NaturalNumber, m: IntegerNumber, theta: Variable, phi: Variable): SphericalHarmonic = (l, m) match {
        case (_, Zero) => apply(l, theta)
        case _ => new LMHarmonic(l, m, theta, phi)
    }

    def apply(l: NaturalNumber, theta: Variable): ZonalSphericalHarmonic = l match {
        case Zero => ZeroZeroHarmonic
        case _ => new LZeroHarmonic(l, theta)
    }

}

class LMHarmonic(val l: NaturalNumber, val m: IntegerNumber, val theta: Variable, val phi: Variable)
        extends SphericalHarmonic {

    protected[this] def f = ???

}