package net.ollie.maths.numbers.complex

import net.ollie.maths.Empty
import net.ollie.maths.functions.angular.RightAngle
import net.ollie.maths.functions.numeric.Signum
import net.ollie.maths.numbers.{One, Real, Zero}

/**
 * Created by Ollie on 12/01/14.
 */
class Imaginary(val coefficient: Real)
        extends Complex {

    /**
     * This is not the coefficient!
     * @return
     */
    def re: Real with Empty = Zero

    def im = this

    override def isEmpty = coefficient.isEmpty

    override def unary_-(): Imaginary = -coefficient

    override def abs = coefficient.abs

    override def arg = RightAngle * Signum(coefficient)

    def +(that: Imaginary): Imaginary = coefficient + that.coefficient

    def -(that: Imaginary): Imaginary = coefficient - that.coefficient

    def *(that: Imaginary): Real = -(coefficient * that.coefficient)

    def /(that: Imaginary): Real = coefficient / that.coefficient

    def *(that: Real): Imaginary = coefficient * that

    override def /(that: Real): Imaginary = coefficient / that

    override def equals(that: Complex): Boolean = that match {
        case i: Imaginary => this.coefficient == i.coefficient
        case _ => that.re == Zero && coefficient == that.im.coefficient
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
        with Empty {

    override def arg = ???

    override def abs = Zero

    override def unary_-() = this

    override def isEmpty = super[Empty].isEmpty

    override def toString = super[Empty].toString

    override def variables = super[Empty].variables

}

object ImaginaryUnit
        extends Imaginary(One) {

    override def toString = "i"

}