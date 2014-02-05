package net.ollie.maths.numbers.complex

import net.ollie.maths.{Empty, EmptyNumber}
import net.ollie.maths.functions.angular.RightAngle
import net.ollie.maths.functions.numeric.Signum
import net.ollie.maths.numbers.{One, Real, Zero}

/**
 * Created by Ollie on 12/01/14.
 */
class Imaginary(val coefficient: Real)
        extends Complex {

    def re: Real with Empty = Zero

    def im = coefficient

    override def isEmpty = coefficient.isEmpty

    override def unary_-(): Imaginary = -coefficient

    override def conjugate: Imaginary = -this

    override def abs = coefficient.abs

    override def arg = RightAngle * Signum(coefficient)

    def *(that: Imaginary): Real = -(coefficient * that.coefficient)

    override def /(that: Real): Imaginary = coefficient / that

    override def equals(that: Complex): Boolean = that match {
        case i: Imaginary => this.coefficient == i.coefficient
        case _ => that.re == Zero && coefficient == that.im
    }

    override def hashCode = -coefficient.hashCode

    override def toString = coefficient.toString + "i"

}

object Imaginary {

    implicit def apply(re: Real): Imaginary = re match {
        case Zero => ImaginaryZero
        case One => ImaginaryUnit
        case _ => new Imaginary(re)
    }

}

object ImaginaryZero
        extends Imaginary(Zero)
        with EmptyNumber {

    override def arg = ???

    override def abs = Zero

    override def unary_-() = this

    override def isEmpty = super[EmptyNumber].isEmpty

    override def variables = super[EmptyNumber].variables

    override def toString = super[EmptyNumber].toString

}

object ImaginaryUnit
        extends Imaginary(One) {

    override def toString = "i"

}