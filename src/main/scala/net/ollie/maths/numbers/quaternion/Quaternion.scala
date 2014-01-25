package net.ollie.maths.numbers.quaternion

import net.ollie.maths._
import net.ollie.maths.numbers.{One, PositiveRealNumber, RealNumber, Zero}
import net.ollie.maths.numbers.complex.ComplexNumber

/**
 * Created by Ollie on 11/01/14.
 */
trait Quaternion
        extends Number {

    type System = Quaternion

    def re: RealNumber

    def i: QuaternionI

    def j: QuaternionJ

    def k: QuaternionK

    override def unary_-(): Quaternion = Quaternion.negate(this)

    def abs: PositiveRealNumber = ???

    def norm: PositiveRealNumber = abs

    def isEmpty = re.isEmpty && i.isEmpty && j.isEmpty && k.isEmpty

    def inverse: Quaternion = conjugate / Quaternion.RealQuaternionArithmetic.convert(norm.squared)

    def conjugate: Quaternion = Quaternion.conjugate(this)

    def isReal: Option[RealNumber] = if (i.isEmpty && j.isEmpty && k.isEmpty) Some(re) else None

    def isComplex: Option[ComplexNumber] = if (j.isEmpty && k.isEmpty) Some(ComplexNumber(re, i.re)) else None

    override def df(x: Variable) = QuaternionZero

    def +(that: Quaternion): Quaternion = Quaternion(re + that.re, i + that.i, j + that.j, k + that.k)

    def *(that: Quaternion): Quaternion = Quaternion(
        re * that.re + i * that.i + j * that.j + k * that.k,
        that.i * re + j * that.k + k * that.j,
        that.j * re + k * that.i + i * that.k,
        that.k * re + i * that.j + j * that.i
    )

    def /(that: Quaternion): Quaternion = this * that.inverse

    def *(that: RealNumber): Quaternion = Quaternion(re * that, i * that, j * that, k * that)

    def /(that: RealNumber): Quaternion = Quaternion(re / that, i / that, j / that, k / that)

    def ?+(that: Number) = that match {
        case re: RealNumber => Some(this + Quaternion(re))
        case z: ComplexNumber => Some(this + Quaternion(z))
        case q: Quaternion => Some(this + q)
        case _ => None
    }

    def ?*(that: Number)(leftToRight: Boolean): Option[Number] = that match {
        case re: RealNumber => Some(this * Quaternion(re))
        case z: ComplexNumber => Some(this * Quaternion(z))
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

    implicit def apply(re: RealNumber): Quaternion = if (re.isEmpty) QuaternionZero else new QuaternionR(re)

    implicit def apply(z: ComplexNumber): Quaternion = Quaternion(z.re, z.im.coefficient, Zero, Zero)

    implicit def i(re: RealNumber): QuaternionI = new QuaternionI(re)

    implicit def j(re: RealNumber): QuaternionJ = new QuaternionJ(re)

    implicit def k(re: RealNumber): QuaternionK = new QuaternionK(re)

    def apply(re: RealNumber, i: QuaternionI, j: QuaternionJ, k: QuaternionK): Quaternion = new FullQuaternion(re, i, j, k)

    def apply(re: Int, ii: Int, jj: Int, kk: Int): Quaternion = apply(re, i(ii), j(jj), k(kk))

    def negate(q: Quaternion): Quaternion = Quaternion(-q.re, -q.i, -q.j, -q.k)

    def conjugate(q: Quaternion): Quaternion = if (q.isEmpty) QuaternionZero else new ConjugatedQuaternion(q)

    def zero: Quaternion with Empty = QuaternionZero

    def one: Quaternion = Quaternion(One)

    implicit object NumberToQuaternion
            extends NumberIdentityArithmetic[Quaternion] {

        def convert(from: Number) = from match {
            case re: RealNumber => Some(Quaternion(re))
            case z: ComplexNumber => Some(z)
            case q: Quaternion => Some(q)
            case _ => None
        }
    }

    implicit object RealQuaternionArithmetic
            extends AdditionArithmetic[RealNumber, Quaternion, Quaternion]
            with MultiplicationArithmetic[RealNumber, Quaternion, Quaternion]
            with IdentityArithmetic[RealNumber, Quaternion] {

        def add(left: RealNumber, right: Quaternion) = Quaternion(left) + right

        def multiply(left: RealNumber, right: Quaternion) = Quaternion(left) * right

        def convert(re: RealNumber) = Quaternion(re)

        def zero = Quaternion.zero

        def one = Quaternion.one

    }

    implicit object ComplexQuaternionArithmetic
            extends AdditionArithmetic[ComplexNumber, Quaternion, Quaternion]
            with MultiplicationArithmetic[ComplexNumber, Quaternion, Quaternion]
            with IdentityArithmetic[ComplexNumber, Quaternion] {

        def add(z: ComplexNumber, q: Quaternion) = Quaternion(z) + q

        def multiply(z: ComplexNumber, q: Quaternion) = Quaternion(z) * q

        def convert(z: ComplexNumber) = z

        def zero = Quaternion.zero

        def one = Quaternion.one

    }

    implicit object QuaternionComplexArithmetic
            extends AdditionArithmetic[Quaternion, ComplexNumber, Quaternion]
            with MultiplicationArithmetic[Quaternion, ComplexNumber, Quaternion] {

        def add(left: Quaternion, right: ComplexNumber) = left + Quaternion(right)

        def multiply(left: Quaternion, right: ComplexNumber) = left * Quaternion(right)

        def zero = Quaternion.zero

        def one = Quaternion.one

    }

}

object QuaternionZero
        extends Quaternion
        with Empty {

    def re = Zero

    def i = Zero

    def j = Zero

    def k = Zero

    override def unary_-() = this

    override def isEmpty = super[Empty].isEmpty

    override def variables = super[Empty].variables

    override def df(x: Variable) = super[Quaternion].df(x)

}

class FullQuaternion(val re: RealNumber, val i: QuaternionI, val j: QuaternionJ, val k: QuaternionK)
        extends Quaternion

class QuaternionR(val re: RealNumber)
        extends Quaternion {

    def i = QuaternionZero.i

    def j = QuaternionZero.j

    def k = QuaternionZero.k

    override def toString = re.toString

}

class QuaternionI(val coefficient: RealNumber)
        extends Quaternion {

    def re = Zero

    def i = this

    def j = Zero

    def k = Zero

    override def unary_-(): QuaternionI = -coefficient

    def +(that: QuaternionI): QuaternionI = coefficient + that.coefficient

    override def *(that: RealNumber): QuaternionI = coefficient * that

    def *(that: QuaternionI): RealNumber = -(coefficient * that.coefficient)

    def *(that: QuaternionJ): QuaternionK = coefficient * that.coefficient

    def *(that: QuaternionK): QuaternionJ = -(coefficient * that.coefficient)

    override def /(that: RealNumber): QuaternionI = coefficient / that

    override def toString: String = coefficient.toString + "i"

    override def hashCode: Int = 13 * coefficient.hashCode

}

class QuaternionJ(val coefficient: RealNumber)
        extends Quaternion {

    def re = Zero

    def i = Zero

    def j = this

    def k = Zero

    override def unary_-(): QuaternionJ = -coefficient

    def +(that: QuaternionJ): QuaternionJ = coefficient + that.coefficient

    override def *(that: RealNumber): QuaternionJ = coefficient * that

    def *(that: QuaternionI): QuaternionK = -(coefficient * that.coefficient)

    def *(that: QuaternionJ): RealNumber = -(coefficient * that.coefficient)

    def *(that: QuaternionK): QuaternionI = coefficient * that.coefficient

    override def /(that: RealNumber): QuaternionJ = coefficient / that

    override def toString: String = coefficient.toString + "j"

    override def hashCode: Int = 17 * coefficient.hashCode

}

class QuaternionK(val coefficient: RealNumber)
        extends Quaternion {

    def re = Zero

    def i = Zero

    def j = Zero

    def k = this

    override def unary_-(): QuaternionK = -coefficient

    def +(that: QuaternionK): QuaternionK = coefficient + that.coefficient

    override def *(that: RealNumber): QuaternionK = coefficient * that

    def *(that: QuaternionI): QuaternionJ = coefficient * that.coefficient

    def *(that: QuaternionJ): QuaternionI = -(coefficient * that.coefficient)

    def *(that: QuaternionK): RealNumber = -(coefficient * that.coefficient)

    override def /(that: RealNumber): QuaternionK = coefficient / that

    override def toString: String = coefficient.toString + "k"

    override def hashCode: Int = 19 * coefficient.hashCode()

}