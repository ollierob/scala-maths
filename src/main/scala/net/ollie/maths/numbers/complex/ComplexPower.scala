package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.{Real, Multivalued}
import net.ollie.maths.Exponentiated
import net.ollie.maths.functions.numeric.{Exp, ComplexLogarithms, Ln}
import net.ollie.maths.functions.angular.Angle
import Angle._

/**
 * Created by Ollie on 25/02/14.
 * @see http://mathworld.wolfram.com/ComplexExponentiation.html
 */
trait ComplexPower
        extends Multivalued
        with Exponentiated {

    type Contents = Complex

    def base: Complex

    def power: Complex

    def inverse = ComplexPower(base, -power)

}

object ComplexPower {

    def apply(base: Complex, power: Complex): ComplexPower = new ComplexPowers(base, power)

}

private class ComplexPowers(val base: Complex, val power: Complex)
        extends ComplexPower {

    private lazy val log: ComplexLogarithms = Ln(base)

    val principal: Complex = new PrincipalComplexPower(base, power)

    private class PrincipalComplexPower(val base: Complex, val power: Complex)
            extends PolarComplex
            with Exponentiated {

        val r: Real = Exp((base * log.principal).re)

        val theta: Angle = (base * log.principal).im radians

        override def isEmpty = super[Exponentiated].isEmpty

    }

}