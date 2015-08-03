package net.ollie.maths.numbers.massive

import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{MaybeReal, Precision, Real}
import net.ollie.maths.{AdditionArithmetic, Constant}

/**
 * Real finite numbers that (probably) cannot be expressed in decimal form because they are so small.
 * Created by Ollie on 09/02/14.
 * @see [[Massive]]
 */
trait Infinitesimal
        extends Constant
        with MaybeReal {

    type System = Infinitesimal

    override def isEmpty = false

    def compareTo(that: Infinitesimal): Int

    def inverse: Constant

    def evaluate(precision: Precision) = 0

    override def unary_-() = ???

    override def ?+(that: Constant): Option[Constant] = that match {
        case re: Real => Some(re)
        case _ => ???
    }

    override def ?*(that: Constant)(leftToRight: Boolean): Option[Constant] = ???

    override def ?^(that: Constant): Option[Constant] = ???

    override def toString = "~0"

}

object Infinitesimal {

    implicit object InfinitesimalRealArithmetic
            extends AdditionArithmetic[Real, Infinitesimal, Real] {

        override def zero = Zero

        override def add(left: Real, right: Infinitesimal) = left

    }

}
