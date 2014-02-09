package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.{PositiveReal, Real}
import net.ollie.maths.Number
import net.ollie.maths.numbers.constants.{Zero, One}

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

    override def equals(z: ComplexLike) = z match {
        case s: SplitComplex => this.equals(s)
        case _ => super.equals(z)
    }

    def equals(s: SplitComplex) = this.re == s.re && this.s == s.s

    override def toString = re.toString + " + " + s.toString + "j"

}

object SplitComplex
        extends ComplexBuilder[SplitComplex] {

    override def apply(n: Number): Option[SplitComplex] = n match {
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

    override def ?^(that: Number): Option[Number] = ???

    override def ?*(that: Number)(leftToRight: Boolean): Option[Number] = ???

    override def ?+(that: Number): Option[Number] = ???

}