package net.ollie.maths.numbers.complex

import net.ollie.maths.functions.angular.{Angle, Cos, Sin}
import net.ollie.maths.numbers.Real

/**
 * Created by Ollie on 12/01/14.
 */
class PolarComplex protected(val r: Real, val theta: Angle)
        extends Complex {

    def re = r * Cos(theta)

    def im = r * Sin(theta)

    override def abs = r.abs

    override def arg = theta

    override def toString = s"$r * e^($theta i)"

}

object PolarComplex {

    def apply(r: Real, theta: Angle): PolarComplex = new PolarComplex(r, theta)

    def apply(z: Complex): PolarComplex = z match {
        case p: PolarComplex => p
        case _ => PolarComplex(z.abs, z.arg)
    }

}