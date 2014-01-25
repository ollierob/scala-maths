package net.ollie.maths.numbers.complex

import scala.Some

import net.ollie.maths._
import net.ollie.maths.Operation.indeterminate
import net.ollie.maths.functions.angular.{Angle, ArcTan}
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers.{One, PositiveRealNumber, RealNumber, Zero}

/**
 * Created by Ollie on 04/01/14.
 */
trait ComplexNumber
        extends Number {

    final type System = ComplexNumber

    def re: RealNumber

    def im: ImaginaryNumber

    override def unary_-(): ComplexNumber = ComplexNumber.negate(this)

    def inverse: ComplexNumber = ComplexNumber.inverse(this)

    def conjugate: ComplexNumber = ComplexNumber.conjugate(this)

    override def df(x: Variable) = ComplexZero

    def abs: PositiveRealNumber = PositiveSquareRoot(re.squared + im.coefficient.squared)

    def arg: Angle = ArcTan(im.re / re)

    def isEmpty: Boolean = re.isEmpty && im.isEmpty

    def toReal: Option[RealNumber] = if (im.isEmpty) Some(re) else None

    def +(that: ComplexNumber): ComplexNumber = ComplexNumber(this.re + that.re, this.im + that.im)

    def -(that: ComplexNumber): ComplexNumber = ComplexNumber(this.re - that.re, this.im - that.im)

    def *(that: ComplexNumber): ComplexNumber = ComplexNumber((this.re * that.re) + (this.im * that.im), (this.im * that.re) + (that.im * this.re))

    def /(that: ComplexNumber): ComplexNumber = this * that.inverse

    def /(that: RealNumber): ComplexNumber = ComplexNumber(this.re / that, this.im / that)

    def ?+(that: Number) = that match {
        case re: RealNumber => Some(this + ComplexNumber(re))
        case z: ComplexNumber => Some(this + z)
        case _ => None
    }

    override def ?*(that: Number)(leftToRight: Boolean) = that match {
        case re: RealNumber => Some(this * ComplexNumber(re))
        case z: ComplexNumber => Some(this * z)
        case _ => None
    }

    def ?^(that: Number) = ???

    override def equals(n: Number): Boolean = n match {
        case z: ComplexNumber => this equals z
        case _ => super.equals(n)
    }

    def equals(that: ComplexNumber) = this.re == that.re && this.im == that.im

    override def hashCode = re.hashCode * im.hashCode

    override def toString = re.toString + " + " + im.toString

}

object ComplexNumber {

    def apply(re: RealNumber, im: RealNumber): ComplexNumber = apply(re, i(im))

    def apply(re: RealNumber, im: ImaginaryNumber): ComplexNumber = if (re.isEmpty && im.isEmpty) ComplexZero else new CartesianComplexNumber(re, im)

    implicit def apply(re: RealNumber): ComplexNumber = apply(re, Zero)

    def negate(z: ComplexNumber): ComplexNumber = ComplexNumber(-z.re, -z.im)

    def inverse(z: ComplexNumber): ComplexNumber = z.conjugate / z.abs.squared

    def conjugate(z: ComplexNumber): ComplexNumber = if (z.isEmpty) ComplexZero else new ComplexConjugate(z)

    def i(re: RealNumber): ImaginaryNumber = ImaginaryNumber(re)

    implicit object NumberToComplex
            extends NumberIdentityArithmetic[ComplexNumber] {

        def convert(from: Number) = from match {
            case re: RealNumber => Some(re)
            case z: ComplexNumber => Some(z)
            case _ => None
        }
    }

    implicit object RealComplexArithmetic
            extends IdentityArithmetic[RealNumber, ComplexNumber]
            with AdditionArithmetic[RealNumber, ComplexNumber, ComplexNumber]
            with MultiplicationArithmetic[RealNumber, ComplexNumber, ComplexNumber] {

        def convert(re: RealNumber) = re

        def add(left: RealNumber, right: ComplexNumber) = ComplexNumber(left) + right

        def multiply(left: RealNumber, right: ComplexNumber) = ComplexNumber(left) * right

        def zero = ComplexZero

        def one = One

    }

    implicit object ComplexRealArithmetic
            extends AdditionArithmetic[ComplexNumber, RealNumber, ComplexNumber]
            with MultiplicationArithmetic[ComplexNumber, RealNumber, ComplexNumber] {

        def add(left: ComplexNumber, right: RealNumber) = left + ComplexNumber(right)

        def multiply(left: ComplexNumber, right: RealNumber) = left * ComplexNumber(right)

        def zero = ComplexZero

        def one = One

    }

}

object ComplexZero
        extends ComplexNumber
        with EmptyNumber {

    def re = Zero

    def im = Zero

    override def unary_-() = this

    override def df(x: Variable) = this

    override def abs = Zero

    override def arg = indeterminate

    override def toString = super[EmptyNumber].toString

}