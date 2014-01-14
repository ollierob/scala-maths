package net.ollie.maths.numbers.complex

import net.ollie.maths.functions.angular.{Angle, Cos, Sin}
import net.ollie.maths.numbers.RealNumber

/**
 * Created by Ollie on 12/01/14.
 */
class PolarComplexNumber(r: RealNumber, theta: Angle)
        extends ComplexNumber {

    def re = r * Cos(theta)

    def im = r * Sin(theta)

    override def abs = r.abs

    override def arg = theta

    override def toString = s"$r * e^($theta i)"

}