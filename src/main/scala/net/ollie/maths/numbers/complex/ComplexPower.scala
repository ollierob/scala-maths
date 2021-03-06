package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.{Integer, Multivalued, Real}
import net.ollie.maths.Variable
import net.ollie.maths.functions.numeric.{ComplexLogarithms, Exp, Ln}
import net.ollie.maths.functions.angular.Angle
import Angle._
import net.ollie.maths.expressions.Exponentiated
import net.ollie.maths.numbers.constants.{One, Zero}

/**
 * Created by Ollie on 25/02/14.
 *
 * @see http://mathworld.wolfram.com/ComplexExponentiation.html
 */
trait ComplexPower
    extends Multivalued
        with Exponentiated {

    type Contents = Complex

    def base: Complex

    def power: Complex

    def inverse = ComplexPower(base, -power)

    override def df(x: Variable) = super[Multivalued].df(x)

    override def toString = "Multivalued(" + super.toString + ")"

}

object ComplexPower {

    def apply(base: Complex, power: Integer): Complex = {
        if (base.isEmpty) Zero
        else if (power.isEmpty) One
        else new ComplexToIntegerPower(base, power)
    }

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

        override def df(x: Variable) = super[PolarComplex].df(x)

        override def isEmpty = super[Exponentiated].isEmpty

    }

}

/**
 *
 */
private class ComplexToIntegerPower(val base: Complex, val power: Integer)
    extends PolarComplex
        with Exponentiated {

    private lazy val p = PolarComplex(base)

    def r = p.r ^ power

    def theta = p.arg * power

    override def isEmpty = super[PolarComplex].isEmpty

    override def df(x: Variable) = super[PolarComplex].df(x)

    override def toString = s"($base ^ $power)"

}