package net.ollie.maths.numbers.complex

import scala.Some

import net.ollie.maths._
import net.ollie.maths.Operation.indeterminate
import net.ollie.maths.functions.angular.{Angle, ArcTan}
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers.{One, PositiveReal, Real, Zero}

/**
 * Created by Ollie on 04/01/14.
 */
trait Complex
        extends Number {

    final type System = Complex

    def re: Real

    def im: Imaginary

    override def unary_-(): Complex = Complex.negate(this)

    def inverse: Complex = Complex.inverse(this)

    def conjugate: Complex = Complex.conjugate(this)

    override def df(x: Variable) = ComplexZero

    def abs: PositiveReal = PositiveSquareRoot(re.squared + im.coefficient.squared)

    def arg: Angle = ArcTan(im.re / re)

    def isEmpty: Boolean = re.isEmpty && im.isEmpty

    def toReal: Option[Real] = if (im.isEmpty) Some(re) else None

    def +(that: Complex): Complex = Complex(this.re + that.re, this.im + that.im)

    def -(that: Complex): Complex = Complex(this.re - that.re, this.im - that.im)

    def *(that: Complex): Complex = Complex((this.re * that.re) + (this.im * that.im), (this.im * that.re) + (that.im * this.re))

    def /(that: Complex): Complex = this * that.inverse

    def /(that: Real): Complex = Complex(this.re / that, this.im / that)

    def ?+(that: Number) = that match {
        case re: Real => Some(this + Complex(re))
        case z: Complex => Some(this + z)
        case _ => None
    }

    override def ?*(that: Number)(leftToRight: Boolean) = that match {
        case re: Real => Some(this * Complex(re))
        case z: Complex => Some(this * z)
        case _ => None
    }

    def ?^(that: Number) = ???

    override def equals(n: Number): Boolean = n match {
        case z: Complex => this equals z
        case _ => super.equals(n)
    }

    def equals(that: Complex) = this.re == that.re && this.im == that.im

    override def hashCode = re.hashCode * im.hashCode

    override def toString = re.toString + " + " + im.toString

}

object Complex {

    def apply(re: Real, im: Real): Complex = apply(re, i(im))

    def apply(re: Real, im: Imaginary): Complex = if (re.isEmpty && im.isEmpty) ComplexZero else new CartesianComplex(re, im)

    implicit def apply(re: Real): Complex = apply(re, Zero)

    def negate(z: Complex): Complex = Complex(-z.re, -z.im)

    def inverse(z: Complex): Complex = z.conjugate / z.abs.squared

    def conjugate(z: Complex): Complex = if (z.isEmpty) ComplexZero else new ComplexConjugate(z)

    def i(re: Real): Imaginary = Imaginary(re)

    implicit object NumberToComplex
            extends NumberIdentityArithmetic[Complex] {

        def convert(from: Number) = from match {
            case re: Real => Some(re)
            case z: Complex => Some(z)
            case _ => None
        }
    }

    implicit object RealComplexArithmetic
            extends IdentityArithmetic[Real, Complex]
            with AdditionArithmetic[Real, Complex, Complex]
            with MultiplicationArithmetic[Real, Complex, Complex] {

        def convert(re: Real) = re

        def add(left: Real, right: Complex) = Complex(left) + right

        def multiply(left: Real, right: Complex) = Complex(left) * right

        def zero = ComplexZero

        def one = One

    }

    implicit object ComplexRealArithmetic
            extends AdditionArithmetic[Complex, Real, Complex]
            with MultiplicationArithmetic[Complex, Real, Complex] {

        def add(left: Complex, right: Real) = left + Complex(right)

        def multiply(left: Complex, right: Real) = left * Complex(right)

        def zero = ComplexZero

        def one = One

    }

}

object ComplexZero
        extends Complex
        with EmptyNumber {

    def re = Zero

    def im = Zero

    override def unary_-() = this

    override def df(x: Variable) = this

    override def abs = Zero

    override def arg = indeterminate

    override def toString = super[EmptyNumber].toString

}