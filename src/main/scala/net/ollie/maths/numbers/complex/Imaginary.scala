package net.ollie.maths.numbers.complex

import net.ollie.maths.{Empty, EmptyConstant}
import net.ollie.maths.functions.angular.RightAngle
import net.ollie.maths.functions.numeric.{Modulo, Signum}
import net.ollie.maths.numbers.{Integer, Real}
import net.ollie.maths.numbers.constants.{Unity, Zero, One}

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

    override def *(that: Real): Imaginary = Imaginary(coefficient * that)

    def *(that: Imaginary): Real = -(coefficient * that.coefficient)

    override def /(that: Real): Imaginary = coefficient / that

    def ^(that: Integer): Complex = {
        val c: Real = coefficient ^ that
        val i: Complex = Modulo(that, 4).remainder.toInt.get match {
            case 0 => One
            case 1 => ImaginaryUnit
            case 2 => -One
            case 3 => -ImaginaryUnit
        }
        i * c
    }

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
        with EmptyConstant {

    override def arg = ???

    override def abs = Zero

    override def unary_-() = this

    override def isEmpty = super[EmptyConstant].isEmpty

    override def variables = super[EmptyConstant].variables

    override def toString = super[EmptyConstant].toString

}

object ImaginaryUnit
        extends Imaginary(One)
        with Unity {

    override def abs = super[Unity].abs

    override def toString = "i"

}