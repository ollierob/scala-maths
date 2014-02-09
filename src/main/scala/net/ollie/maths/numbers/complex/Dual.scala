package net.ollie.maths.numbers.complex

import net.ollie.maths._
import net.ollie.maths.numbers.Real
import scala.Some
import net.ollie.maths.numbers.constants.{Zero, One}

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

    def ?+(that: Number) = that match {
        case d: Dual => Some(this + d)
        case _ => None
    }

    def ?*(that: Number)(leftToRight: Boolean) = that match {
        case d: Dual => Some(this * d)
        case _ => None
    }

    def ?^(that: Number) = None

    override def equals(z: ComplexLike) = z match {
        case d: Dual => this equals d
        case _ => super.equals(z)
    }

    def equals(that: Dual): Boolean = (this.re == that.re && this.d == that.d) || super.equals(that)

    override def toString = re.toString + " + " + d.toString + "d"

}

object Dual
        extends ComplexBuilder[Dual] {

    def unitSquared = Zero

    def apply(n: Number): Option[Dual] = n match {
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
            with IdentityArithmetic[Real, Dual] {

        override def zero = DualZero

        override def one = One

        override def promote(from: Real) = Dual(from)

        override def add(left: Real, right: Dual) = promote(left) + right

        override def multiply(left: Real, right: Dual) = promote(left) * right

    }

    def at(function: Univariate, d: Dual): Number = {
        function(d.re)(Real) + (d.d * function.dx(d.d)(Real))
    }

}

class RegularDual(val re: Real, val d: Real)
        extends Dual

object DualZero
        extends Dual
        with EmptyNumber {

    override def re = Zero

    override def d = Zero

    override def abs = super[EmptyNumber].abs

    override def unary_-() = this

    override def conjugate = this

}
