package net.ollie.maths.numbers.quaternion

import net.ollie.maths._
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{MaybeComplex, Complex}
import scala.Some
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 11/01/14.
 */
trait Quaternion
        extends Number
        with MaybeComplex {

    type System = Quaternion

    type Inverse = Quaternion

    def re: Real

    def i: QuaternionI

    def j: QuaternionJ

    def k: QuaternionK

    override def unary_-(): Quaternion = Quaternion.negate(this)

    def abs: PositiveReal = ???

    def norm: PositiveReal = abs

    def isEmpty = re.isEmpty && i.isEmpty && j.isEmpty && k.isEmpty

    def inverse: Quaternion = conjugate / Quaternion.RealQuaternionArithmetic.promote(norm.squared)

    def conjugate: Quaternion = Quaternion.conjugate(this)

    override def toReal: Option[Real] = if (i.isEmpty && j.isEmpty && k.isEmpty) Some(re) else None

    def toComplex: Option[Complex] = if (j.isEmpty && k.isEmpty) Some(Complex(re, i.coefficient)) else None

    override def df(x: Variable) = QuaternionZero

    def +(that: Quaternion): Quaternion = Quaternion(re + that.re, i + that.i, j + that.j, k + that.k)

    def *(that: Quaternion): Quaternion = Quaternion(
        re * that.re + i * that.i + j * that.j + k * that.k,
        that.i * re + j * that.k + k * that.j,
        that.j * re + k * that.i + i * that.k,
        that.k * re + i * that.j + j * that.i
    )

    def /(that: Quaternion): Quaternion = this * that.inverse

    def *(that: Real): Quaternion = Quaternion(re * that, i * that, j * that, k * that)

    def /(that: Real): Quaternion = Quaternion(re / that, i / that, j / that, k / that)

    def ?+(that: Number) = that match {
        case re: Real => Some(this + Quaternion(re))
        case z: Complex => Some(this + Quaternion(z))
        case q: Quaternion => Some(this + q)
        case _ => None
    }

    def ?*(that: Number)(leftToRight: Boolean): Option[Number] = that match {
        case re: Real => Some(this * Quaternion(re))
        case z: Complex => Some(this * Quaternion(z))
        case q: Quaternion => Some(this * q)
        case _ => None
    }

    def ?^(that: Number) = ???

    override def toString: String = re.toString + " + " + i.toString + " + " + j.toString + " + " + k.toString

    override def equals(n: Number) = n match {
        case q: Quaternion => this.re == q.re
        case _ => super.equals(n)
    }

    override def hashCode = re.hashCode + 3 * i.hashCode + 7 * j.hashCode + 11 * k.hashCode

}

object Quaternion {

    implicit def apply(re: Real): Quaternion = if (re.isEmpty) QuaternionZero else new QuaternionR(re)

    implicit def apply(z: Complex): Quaternion = Quaternion(z.re, z.im, Zero, Zero)

    implicit def i(re: Real): QuaternionI = new QuaternionI(re)

    implicit def j(re: Real): QuaternionJ = new QuaternionJ(re)

    implicit def k(re: Real): QuaternionK = new QuaternionK(re)

    def apply(n: Number): Option[Quaternion] = n match {
        case re: Real => Some(Quaternion(re))
        case z: Complex => Some(Quaternion(z))
        case q: Quaternion => Some(q)
        case _ => None
    }

    def apply(re: Real, i: QuaternionI, j: QuaternionJ, k: QuaternionK): Quaternion = new CartesianQuaternion(re, i, j, k)

    def apply(re: Int, ii: Int, jj: Int, kk: Int): Quaternion = apply(re, i(ii), j(jj), k(kk))

    def negate(q: Quaternion): Quaternion = Quaternion(-q.re, -q.i, -q.j, -q.k)

    def conjugate(q: Quaternion): Quaternion = if (q.isEmpty) QuaternionZero else new ConjugatedQuaternion(q)

    def zero: Quaternion with Empty = QuaternionZero

    def one: Quaternion = Quaternion(One)

    implicit object RealQuaternionArithmetic
            extends AdditionArithmetic[Real, Quaternion, Quaternion]
            with MultiplicationArithmetic[Real, Quaternion, Quaternion]
            with IdentityArithmetic[Real, Quaternion] {

        def add(left: Real, right: Quaternion) = Quaternion(left) + right

        def multiply(left: Real, right: Quaternion) = Quaternion(left) * right

        def promote(re: Real) = Quaternion(re)

        def zero = Quaternion.zero

        def one = Quaternion.one

    }

    implicit object ComplexQuaternionArithmetic
            extends AdditionArithmetic[Complex, Quaternion, Quaternion]
            with MultiplicationArithmetic[Complex, Quaternion, Quaternion]
            with IdentityArithmetic[Complex, Quaternion] {

        def add(z: Complex, q: Quaternion) = Quaternion(z) + q

        def multiply(z: Complex, q: Quaternion) = Quaternion(z) * q

        def promote(z: Complex) = z

        def zero = Quaternion.zero

        def one = Quaternion.one

    }

    implicit object QuaternionComplexArithmetic
            extends AdditionArithmetic[Quaternion, Complex, Quaternion]
            with MultiplicationArithmetic[Quaternion, Complex, Quaternion] {

        def add(left: Quaternion, right: Complex) = left + Quaternion(right)

        def multiply(left: Quaternion, right: Complex) = left * Quaternion(right)

        def zero = Quaternion.zero

        def one = Quaternion.one

    }

}

object QuaternionZero
        extends Quaternion
        with EmptyNumber {

    def re = Zero

    def i = Zero

    def j = Zero

    def k = Zero

    override def abs = Zero

    override def unary_-() = this

    override def isEmpty = super[EmptyNumber].isEmpty

    override def variables = super[EmptyNumber].variables

    override def df(x: Variable) = super[Quaternion].df(x)

}

class QuaternionR(val re: Real)
        extends Quaternion {

    def i = QuaternionZero.i

    def j = QuaternionZero.j

    def k = QuaternionZero.k

    override def toString = re.toString

}

class QuaternionI(val coefficient: Real)
        extends Quaternion {

    def re = Zero

    def i = this

    def j = Zero

    def k = Zero

    override def unary_-(): QuaternionI = -coefficient

    def +(that: QuaternionI): QuaternionI = coefficient + that.coefficient

    override def *(that: Real): QuaternionI = coefficient * that

    def *(that: QuaternionI): Real = -(coefficient * that.coefficient)

    def *(that: QuaternionJ): QuaternionK = coefficient * that.coefficient

    def *(that: QuaternionK): QuaternionJ = -(coefficient * that.coefficient)

    override def /(that: Real): QuaternionI = coefficient / that

    override def toString: String = coefficient.toString + "i"

    override def hashCode: Int = 13 * coefficient.hashCode

}

class QuaternionJ(val coefficient: Real)
        extends Quaternion {

    def re = Zero

    def i = Zero

    def j = this

    def k = Zero

    override def unary_-(): QuaternionJ = -coefficient

    def +(that: QuaternionJ): QuaternionJ = coefficient + that.coefficient

    override def *(that: Real): QuaternionJ = coefficient * that

    def *(that: QuaternionI): QuaternionK = -(coefficient * that.coefficient)

    def *(that: QuaternionJ): Real = -(coefficient * that.coefficient)

    def *(that: QuaternionK): QuaternionI = coefficient * that.coefficient

    override def /(that: Real): QuaternionJ = coefficient / that

    override def toString: String = coefficient.toString + "j"

    override def hashCode: Int = 17 * coefficient.hashCode

}

class QuaternionK(val coefficient: Real)
        extends Quaternion {

    def re = Zero

    def i = Zero

    def j = Zero

    def k = this

    override def unary_-(): QuaternionK = -coefficient

    def +(that: QuaternionK): QuaternionK = coefficient + that.coefficient

    override def *(that: Real): QuaternionK = coefficient * that

    def *(that: QuaternionI): QuaternionJ = coefficient * that.coefficient

    def *(that: QuaternionJ): QuaternionI = -(coefficient * that.coefficient)

    def *(that: QuaternionK): Real = -(coefficient * that.coefficient)

    override def /(that: Real): QuaternionK = coefficient / that

    override def toString: String = coefficient.toString + "k"

    override def hashCode: Int = 19 * coefficient.hashCode()

}