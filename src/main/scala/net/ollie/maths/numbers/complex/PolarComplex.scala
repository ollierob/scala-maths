package net.ollie.maths.numbers.complex

import net.ollie.maths.functions.angular.{Angle, Cos, Sin}
import net.ollie.maths.numbers.Real

/**
 * Created by Ollie on 12/01/14.
 */
trait PolarComplex
        extends Complex {

    def r: Real

    def theta: Angle

    override def abs = r.abs

    override def arg = theta

    def re = abs * Cos(arg)

    def im = abs * Sin(arg)

    override def toString = s"$abs * e^($arg i)"

}

object PolarComplex {

    def apply(r: Real, theta: Angle): PolarComplex = new PolarComplexOf(r, theta)

    def apply(z: Complex): PolarComplex = z match {
        case p: PolarComplex => p
        case _ => PolarComplex(z.abs, z.arg)
    }

}

private class PolarComplexOf(val r: Real, val theta: Angle)
        extends PolarComplex