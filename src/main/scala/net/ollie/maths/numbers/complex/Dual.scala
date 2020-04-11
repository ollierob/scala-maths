package net.ollie.maths.numbers.complex

import net.ollie.maths._
import net.ollie.maths.expressions.{Empty, Univariate}
import net.ollie.maths.numbers.{EmptyConstant, Real}

import scala.Some
import net.ollie.maths.numbers.constants.{Unity, Zero}

/**
 * Created by Ollie on 28/01/14.
 * @see http://mathworld.wolfram.com/DualNumber.html
 */
trait Dual
        extends ComplexLike {

    type System = Dual

    override implicit protected[this] val builder = Dual

    def d: Real

    final def unre = d

    override def unary_-(): Dual = Dual(-re, -d)

    def abs = re.abs

    def ?+(that: Constant) = that match {
        case d: Dual => Some(this + d)
        case _ => None
    }

    def ?*(that: Constant)(leftToRight: Boolean) = that match {
        case d: Dual => Some(this * d)
        case _ => None
    }

    def ?^(that: Constant) = None

    override def equals(z: ComplexLike) = z match {
        case d: Dual => this equals d
        case _ => super.equals(z)
    }

    def equals(that: Dual): Boolean = (this.re == that.re && this.d == that.d) || super.equals(that)

    override def toString = re.toString + " + " + d.toString + "d"

}

object Dual
        extends ComplexLikeBuilder[Dual] {

    def zero: Dual with Empty = DualZero

    def one: Dual with Unity = DualOne

    def unitSquared = Zero

    def apply(n: Constant): Option[Dual] = n match {
        case re: Real => Some(Dual(re))
        case d: Dual => Some(d)
        case _ => None
    }

    implicit def apply(re: Real): Dual = Dual(re, Zero)

    def apply(re: Real, d: Real): Dual = if (re.isEmpty && d.isEmpty) DualZero else new RegularDual(re, d)

    implicit def apply(pair: (Real, Real)): Dual = Dual(pair._1, pair._2)

    implicit object RealDualArithmetic
            extends AdditionArithmetic[Real, Dual, Dual]
            with MultiplicationArithmetic[Real, Dual, Dual]
            with NumberConversionArithmetic[Real, Dual] {

        override def zero = Dual.zero

        override def one = Dual.one

        override def apply(from: Real) = Dual(from)

        override def add(left: Real, right: Dual) = apply(left) + right

        override def multiply(left: Real, right: Dual) = apply(left) * right

    }

    def at(function: Univariate, d: Dual): Constant = {
        function(d.re)(Real) + (d.d * function.dx(d.d)(Real))
    }

}

class RegularDual(val re: Real, val d: Real)
        extends Dual

object DualZero
        extends Dual
        with EmptyConstant {

    override def re = Zero

    override def d = Zero

    override def abs = super[EmptyConstant].abs

    override def unary_-() = this

    override def conjugate = this

}

object DualOne
        extends RegularDual(1, 0)
        with Unity {

    override def abs = super[Unity].abs

}