package net.ollie.maths.numbers.complex

import net.ollie.maths.Empty
import net.ollie.maths.functions.angular.RightAngle
import net.ollie.maths.functions.numeric.Signum
import net.ollie.maths.numbers.{One, RealNumber, Zero}

/**
 * Created by Ollie on 12/01/14.
 */
class ImaginaryNumber(val coefficient: RealNumber)
        extends ComplexNumber {

    /**
     * This is not the coefficient!
     * @return
     */
    def re: RealNumber with Empty = Zero

    def im = this

    override def isEmpty = coefficient.isEmpty

    override def unary_-(): ImaginaryNumber = -coefficient

    override def abs = coefficient.abs

    override def arg = RightAngle * Signum(coefficient)

    def +(that: ImaginaryNumber): ImaginaryNumber = coefficient + that.coefficient

    def -(that: ImaginaryNumber): ImaginaryNumber = coefficient - that.coefficient

    def *(that: ImaginaryNumber): RealNumber = -(coefficient * that.coefficient)

    def /(that: ImaginaryNumber): RealNumber = coefficient / that.coefficient

    def *(that: RealNumber): ImaginaryNumber = coefficient * that

    override def /(that: RealNumber): ImaginaryNumber = coefficient / that

    override def equals(that: ComplexNumber): Boolean = that match {
        case i: ImaginaryNumber => this.coefficient == i.coefficient
        case _ => that.re == Zero && coefficient == that.im.coefficient
    }

    override def hashCode = -coefficient.hashCode

    override def toString = coefficient.toString + "i"

}

object ImaginaryNumber {

    implicit def apply(re: RealNumber): ImaginaryNumber = re match {
        case Zero => ImaginaryZero
        case One => ImaginaryUnit
        case _ => new ImaginaryNumber(re)
    }

}

object ImaginaryZero
        extends ImaginaryNumber(Zero)
        with Empty {

    override def arg = ???

    override def abs = Zero

    override def unary_-() = this

    override def isEmpty = super[Empty].isEmpty

    override def toString = super[Empty].toString

    override def variables = super[Empty].variables

}

object ImaginaryUnit
        extends ImaginaryNumber(One) {

    override def toString = "i"

}