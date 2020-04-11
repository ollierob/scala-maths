package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.constants.{One, Unity, Zero}
import net.ollie.maths.numbers.{EmptyConstant, MaybeReal, Real}
import net.ollie.maths.Constant
import net.ollie.maths.expressions.Empty

/**
 * Created by Ollie on 04/02/14.
 *
 * @see <a href="https://en.wikipedia.org/wiki/Split-complex_number">Wikipedia</a>
 */
trait SplitComplex
    extends ComplexLike {

    type System = SplitComplex

    override implicit val builder = SplitComplex

    def s: Real

    override def unre = s

    def modulus: Real = re.squared - s.squared

    def ?+(that: Constant): Option[SplitComplex] = that match {
        case z: SplitComplex => Some(this + z)
        case r: Real => Some(this + r)
        case m: MaybeReal => m.toReal.map(r => this + r)
        case _ => None
    }

    def ?*(that: Constant)(leftToRight: Boolean): Option[Constant] = that match {
        case z: SplitComplex => Some(SplitComplex(re * z.re + s * z.s, re * z.s + s * z.re))
        case r: Real => Some(this * r)
        case m: MaybeReal => m.toReal.map(r => this * r)
        case _ => None
    }

    def ?^(that: Constant): Option[Constant] = ???

    override def equals(z: ComplexLike) = z match {
        case s: SplitComplex => this.equals(s)
        case _ => super.equals(z)
    }

    def equals(s: SplitComplex) = this.re == s.re && this.s == s.s

    override def toString = re.toString + " + " + s.toString + "j"

}

object SplitComplex
    extends ComplexLikeBuilder[SplitComplex] {

    def zero: SplitComplex with Empty = SplitComplexZero

    def one: SplitComplex with Unity = ???

    override def apply(n: Constant): Option[SplitComplex] = n match {
        case re: Real => Some(re)
        case z: SplitComplex => Some(z)
        case z: MaybeReal => z.toReal.map(r => SplitComplex(r))
        case _ => None
    }

    implicit def apply(re: Real): SplitComplex = apply((re, Zero))

    def apply(pair: (Real, Real)) = new CartesianSplitComplex(pair._1, pair._2)

    def unitSquared = One

}

class CartesianSplitComplex(val re: Real, val s: Real)
    extends SplitComplex {

    override def ?^(that: Constant): Option[Constant] = ???

    override def ?*(that: Constant)(leftToRight: Boolean): Option[Constant] = ???

    override def ?+(that: Constant): Option[SplitComplex] = that match {
        case c: CartesianSplitComplex => Some(new CartesianSplitComplex(re + c.re, s + c.s))
        case _ => super.?+(that)
    }

}

private object SplitComplexZero
    extends SplitComplex with EmptyConstant {

    def re = Zero

    def s = Zero

    override def ?+(that: Constant) = SplitComplex(that)

    override def ?*(that: Constant)(leftToRight: Boolean) = Some(this)

    override def abs = super[EmptyConstant].abs

}

private object SplitComplexOne
    extends SplitComplex with Unity {

    override def abs = super[Unity].abs

    def re = One

    def s = Zero

}