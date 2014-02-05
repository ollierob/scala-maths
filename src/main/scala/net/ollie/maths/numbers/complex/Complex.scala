package net.ollie.maths.numbers.complex

import net.ollie.maths._
import net.ollie.maths.Operation.indeterminate
import net.ollie.maths.functions.angular.{Angle, ArcTan}
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers._
import scala.Some

/**
 * Created by Ollie on 04/01/14.
 */
trait Complex
        extends ComplexLike {

    final type System = Complex

    override implicit val builder = Complex

    def im: Real

    def unre = im

    override def df(x: Variable) = ComplexZero

    def abs: PositiveReal = PositiveSquareRoot(re.squared + im.squared)

    def arg: Angle = ArcTan(im / re)

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

    override def equals(z: ComplexLike): Boolean = z match {
        case z: Complex => this equals z
        case _ => super.equals(z)
    }

    def equals(that: Complex) = this.re == that.re && this.im == that.im

    override def hashCode = re.hashCode * im.hashCode

}

object Complex
        extends ComplexBuilder[Complex] {

    override def unitSquared = MinusOne

    def apply(n: Number): Option[Complex] = n match {
        case re: Real => Some(Complex(re))
        case z: Complex => Some(z)
        case x: ComplexLike => x.toReal match {
            case Some(re) => Some(re)
            case _ => None
        }
        case _ => None
    }

    implicit def apply(pair: (Real, Real)): Complex = pair match {
        case (Zero, Zero) => ComplexZero
        case (Zero, _) => Imaginary(pair._2)
        case _ => new CartesianComplex(pair._1, pair._2)
    }

    def apply(re: Real, im: Real): Complex = Complex((re, im))

    implicit def apply(re: Real): Complex = Complex(re, Zero)

    implicit object RealComplexArithmetic
            extends IdentityArithmetic[Real, Complex]
            with AdditionArithmetic[Real, Complex, Complex]
            with MultiplicationArithmetic[Real, Complex, Complex] {

        def promote(re: Real) = re

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