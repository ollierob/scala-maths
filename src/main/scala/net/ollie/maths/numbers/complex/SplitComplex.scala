package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.{Zero, One, PositiveReal, Real}
import net.ollie.maths.Number

/**
 * Created by Ollie on 04/02/14.
 */
trait SplitComplex
        extends ComplexLike {

    type System = SplitComplex

    override implicit val builder = SplitComplex

    def s: Real

    def unre = s

    override def abs: PositiveReal = ???

    def modulus: Real = re.squared - s.squared

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

    override protected def ?*(that: Number)(leftToRight: Boolean): Option[Number] = ???

    override def ?+(that: Number): Option[Number] = ???

}