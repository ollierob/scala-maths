package net.ollie.maths.numbers.complex

import net.ollie.maths._
import net.ollie.maths.Operation.indeterminate
import net.ollie.maths.functions.angular.{Angle, ArcTan}
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers._
import scala.Some
import net.ollie.maths.numbers.constants.{Unity, MinusOne, Zero}

/**
 * Created by Ollie on 04/01/14.
 */
trait Complex
        extends ComplexLike
        with MaybeComplex {

    type System = Complex

    override implicit val builder = Complex

    def im: Real

    def unre = im

    override def df(x: Variable) = ComplexZero

    def abs: PositiveReal = PositiveSquareRoot(re.squared + im.squared)

    def arg: Angle = ArcTan(im / re)

    def toComplex = Some(this)

    override def toReal = super[MaybeComplex].toReal

    def ^(that: Complex): ComplexPower = Complex.pow(this, that)

    def ?+(that: Constant) = that match {
        case re: Real => Some(this + Complex(re))
        case z: Complex => Some(this + z)
        case _ => None
    }

    override def ?*(that: Constant)(leftToRight: Boolean) = that match {
        case re: Real => Some(this * Complex(re))
        case z: Complex => Some(this * z)
        case _ => None
    }

    def ?^(that: Constant) = None

    override def equals(z: ComplexLike): Boolean = z match {
        case z: Complex => this equals z
        case _ => super.equals(z)
    }

    def equals(that: Complex) = {
        this.re == that.re && this.im == that.im
    }

    override def hashCode = re.hashCode * im.hashCode

}

object Complex
        extends ComplexLikeBuilder[Complex] {

    override def unitSquared = MinusOne

    def zero: Complex with EmptyConstant = ComplexZero

    def one: Complex with Unity = ComplexOne

    def apply(n: Constant): Option[Complex] = n match {
        case re: Real => Some(Complex(re))
        case z: Complex => Some(z)
        case m: MaybeComplex => m.toComplex
        case x: ComplexLike => x.toReal match {
            case Some(re) => Some(re)
            case _ => None
        }
        case _ => None
    }

    implicit def apply(pair: (Real, Real)): Complex = pair match {
        case (Zero, Zero) => zero
        case (Zero, _) => Imaginary(pair._2)
        case _ => new CartesianComplex(pair._1, pair._2)
    }

    def apply(re: Real, im: Real): Complex = Complex((re, im))

    implicit def apply(re: Real): Complex = {
        if (re.isEmpty) zero
        else Complex(re, Zero)
    }

    def pow(base: Complex, power: Complex): ComplexPower = ComplexPower(base, power)

    def polar(r: Real, theta: Angle): PolarComplex = PolarComplex(r, theta)

    implicit object RealComplexArithmetic
            extends NumberConversionArithmetic[Real, Complex]
            with AdditionArithmetic[Real, Complex, Complex]
            with MultiplicationArithmetic[Real, Complex, Complex]
            with ExponentiationArithmetic[Real, Complex, ComplexPower] {

        def apply(re: Real) = re

        def zero = Complex.zero

        def one = Complex.one

        def add(left: Real, right: Complex) = Complex(left) + right

        def multiply(left: Real, right: Complex) = Complex(left) * right

        def exponentiate(base: Real, power: Complex) = Complex(base) ^ power

    }

    implicit object ComplexRealArithmetic
            extends AdditionArithmetic[Complex, Real, Complex]
            with MultiplicationArithmetic[Complex, Real, Complex]
            with ExponentiationArithmetic[Complex, Real, ComplexPower] {

        def zero = Complex.zero

        def one = Complex.one

        def add(left: Complex, right: Real) = left + Complex(right)

        def multiply(left: Complex, right: Real) = left * Complex(right)

        def exponentiate(base: Complex, power: Real) = base ^ Complex(power)

    }

}

trait MaybeComplex
        extends MaybeReal {

    def toReal = this toComplex match {
        case Some(z) => if (z.im.isEmpty) Some(z.re) else None
        case _ => None
    }

    def toComplex: Option[Complex]

}

object ComplexZero
        extends Complex
        with EmptyConstant {

    def re = Zero

    def im = Zero

    override def unary_-() = this

    override def df(x: Variable) = this

    override def abs = Zero

    override def arg = indeterminate

    override def toString = super[EmptyConstant].toString

}

private object ComplexOne
        extends CartesianComplex(1, 0)
        with Unity {

    override def abs = super[Unity].abs

}
