package net.ollie.maths.numbers.complex

import net.ollie.maths._
import net.ollie.maths.numbers.{PositiveReal, One, Real, Zero}
import scala.Some

/**
 * Created by Ollie on 28/01/14.
 * @see http://mathworld.wolfram.com/DualNumber.html
 */
trait Dual
        extends Number {

    type System = Dual

    def re: Real

    def d: Dualistic

    override def isEmpty = re.isEmpty && d.isEmpty

    def inverse: Dual = Dual(re.inverse, (-d.coefficient) / (re.squared))

    override def unary_-(): Dual = Dual(-re, -d)

    def ?+(that: Number) = that match {
        case d: Dual => Some(this + d)
        case _ => None
    }

    def ?*(that: Number)(leftToRight: Boolean) = that match {
        case d: Dual => Some(this * d)
        case _ => None
    }

    def ?^(that: Number) = None

    def +(that: Real): Dual = this + Dual(that)

    def +(that: Dual): Dual = Dual(this.re + that.re, this.d + that.d)

    def -(that: Dual): Dual = this + (-that)

    def *(that: Real): Dual = this * Dual(that)

    def *(that: Dual): Dual = {
        val re: Real = (this.re * that.re) + (this.d * that.d)
        val d: Dualistic = (this.d * that.re) + (that.d * this.re)
        Dual(re, d)
    }

    def /(that: Dual): Dual = {
        val re: Real = this.re / that.re
        val d: Dualistic = ((this.d.coefficient * that.re) - (this.re * that.d.coefficient)) / (that.re.squared)
        Dual(re, d)
    }

    def /(that: Real): Dual = Dual(this.re / that, this.d / that)

    override def equals(that: Number) = that match {
        case re: Real => this.d.isEmpty && this.re == re
        case d: Dual => this.equals(d)
        case _ => super.equals(that)
    }

    def equals(that: Dual): Boolean = (this.re == that.re && this.d == that.d) || super.equals(that)

    override def toString: String = s"$re + $d"

}

object Dual {

    def apply(n: Number): Option[Dual] = n match {
        case re: Real => Some(Dual(re))
        case d: Dual => Some(d)
        case _ => None
    }

    implicit def apply(re: Real): Dual = Dual(re, Zero)

    def apply(re: Int, d: Int): Dual = apply(re, Dualistic(d))

    def apply(re: Real, d: Dualistic): Dual = if (re.isEmpty) d else new RegularDual(re, d)

    implicit object RealDualArithmetic
            extends AdditionArithmetic[Real, Dual, Dual]
            with MultiplicationArithmetic[Real, Dual, Dual]
            with IdentityArithmetic[Real, Dual] {

        override def zero = DualZero

        override def one = One

        override def promote(from: Real) = Dual(from)

        override def add(left: Real, right: Dual) = promote(left) + right

        override def multiply(left: Real, right: Dual) = promote(left) * right

        override def convert(n: Number) = Dual(n)

    }

    def at(function: Univariate, d: Dual): Number = {
        function(d.re) + (d.d.coefficient * function.dx(d.d.coefficient))
    }

}

class RegularDual(val re: Real, val d: Dualistic)
        extends Dual {

    override def abs: PositiveReal = ???

}

object DualZero
        extends Dual
        with EmptyNumber {

    override def re = Zero

    override def d = Zero

}

object Dualistic {

    implicit def apply(re: Real): Dualistic = new Dualistic(re)

    implicit object Multiplication
            extends MultiplicationArithmetic[Dualistic, Dualistic, Real] {

        override def one = 1

        override def zero = Zero

        override def multiply(left: Dualistic, right: Dualistic) = left * right

    }

}

class Dualistic(val coefficient: Real)
        extends Dual {

    override type System = Dual

    override final def re = Zero

    override final def d = this

    override def isEmpty = coefficient.isEmpty

    override def unary_-(): Dualistic = Dualistic(-coefficient)

    def +(that: Dualistic): Dualistic = Dualistic(this.coefficient + that.coefficient)

    def *(that: Dualistic): Real = Zero

    override def *(re: Real): Dualistic = Dualistic(this.coefficient * re)

    override def /(re: Real): Dualistic = Dualistic(this.coefficient / re)

    override def abs = re.abs

    override def equals(that: Dual) = that match {
        case d: Dualistic => (this.coefficient == d.coefficient) || super.equals(d)
        case _ => super.equals(that)
    }

    override def toString = coefficient.toString + "d"

}

object DualUnit
        extends Dualistic(One) {

    override def toString = "d"

}
