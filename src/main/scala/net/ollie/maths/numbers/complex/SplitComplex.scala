package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.{PositiveReal, Real}
import net.ollie.maths.{EmptyConstant, Empty, Constant}
import net.ollie.maths.numbers.constants.{Unity, Zero, One}

/**
 * Created by Ollie on 04/02/14.
 */
trait SplitComplex
        extends ComplexLike {

    type System = SplitComplex

    override implicit val builder = SplitComplex

    def s: Real

    def unre = s

    def abs: PositiveReal = ??? //TODO

    def modulus: Real = re.squared - s.squared

    def ?^(that: Constant): Option[Constant] = ???

    def ?*(that: Constant)(leftToRight: Boolean): Option[Constant] = ???

    def ?+(that: Constant): Option[Constant] = ???

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

    override def ?+(that: Constant): Option[Constant] = ???

}

private object SplitComplexZero
        extends SplitComplex
        with EmptyConstant {

    def re = Zero

    def s = Zero

    override def abs = super[EmptyConstant].abs

}

private object SplitComplexOne
        extends SplitComplex
        with Unity {

    override def abs = super[Unity].abs

    def re = One

    def s = Zero

}