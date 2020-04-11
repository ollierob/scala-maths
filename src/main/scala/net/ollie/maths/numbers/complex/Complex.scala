package net.ollie.maths.numbers.complex

import net.ollie.maths.Operation.indeterminate
import net.ollie.maths._
import net.ollie.maths.functions.angular.{Angle, ArcTan}
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{MinusOne, Unity, Zero}

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

    def isReal = im.isZero

    override def df(x: Variable) = ComplexZero

    def abs: PositiveReal = PositiveSquareRoot(re.squared + im.squared)

    def arg: Angle = ArcTan(im / re)

    def toComplex = Some(this)

    override def toReal = super[MaybeComplex].toReal

    def isGaussian = re.isInteger && im.isInteger

    override def ^(that: Expression) = Complex(that.toConstant) match {
        case Some(z) => this ^ z
        case _ => super.^(that)
    }

    def ^(that: Real): ComplexPower = this ^ Complex(that)

    def ^(that: Integer): Complex = Complex.pow(this, that)

    def ^(that: Complex): ComplexPower = Complex.pow(this, that)

    def ?+(that: Constant) = that match {
        case r: Real => Some(this + r)
        case z: Complex => Some(this + z)
        case _ => Complex(that).map(z => this + z)
    }

    override def ?*(that: Constant)(leftToRight: Boolean) = that match {
        case r: Real => Some(this * Complex(r))
        case z: Complex => Some(this * z)
        case _ => Complex(that).map(z => this * z)
    }

    def ?^(that: Constant) = ???

    override def equals(z: ComplexLike): Boolean = z match {
        case z: Complex => this equals z
        case _ => super.equals(z)
    }

    def equals(that: Complex) = {
        this.re == that.re && this.im == that.im
    }

    override def hashCode = re.hashCode + im.hashCode

}

object Complex
    extends ComplexLikeBuilder[Complex] {

    override def unitSquared = MinusOne

    val zero: Complex with EmptyConstant = ComplexZero

    def one: Complex with Unity = ComplexOne

    def apply(n: Constant): Option[Complex] = n match {
        case re: Real => Some(Complex(re))
        case z: Complex => Some(z)
        case m: MaybeComplex => m.toComplex
        case m: MaybeReal => m.toReal match {
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

    def isGaussian(c: Constant): Boolean = Complex(c) exists (z => z.isGaussian)

    def pow(base: Complex, power: Integer): Complex = ComplexPower(base, power)

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
    extends Complex with EmptyConstant {

    def re = Zero

    def im = Zero

    override def unary_-() = this

    override def df(x: Variable) = this

    override def abs = Zero

    override def arg = indeterminate

    override def toString = super[EmptyConstant].toString

}

private object ComplexOne
    extends CartesianComplex(re = 1, im = 0)
        with Unity {

    override def abs = super[Unity].abs

}
