package net.ollie.maths.numbers.massive

import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{MaybeReal, Real}
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

    override def inverse: Massive = new InverseInfinitesimal(this)

    override def unary_-(): Infinitesimal = new NegatedInfinitesimal(this)

    override def ?+(that: Constant): Option[Constant] = that match {
        case i: Infinitesimal => Some(this + i)
        case _ => that ?+ this.closestReal
    }

    def toReal: Option[Real] = Some(closestReal)

    def closestReal: Real = Zero

    def +(that: Real): Real = that + closestReal

    def +(that: Infinitesimal): Infinitesimal = new InfinitesimalSeries(this, that)

    override def ?*(that: Constant)(leftToRight: Boolean): Option[Constant] = ???

    override def ?^(that: Constant): Option[Constant] = ???

    override def toString = "~0"

}

object Infinitesimal {

    implicit object InfinitesimalRealArithmetic
            extends AdditionArithmetic[Real, Infinitesimal, Real] {

        override def zero = Zero

        override def add(left: Real, right: Infinitesimal) = right + left

    }

}

class InfinitesimalSeries(items: List[Infinitesimal])
        extends Infinitesimal {

    def this(x: Infinitesimal, y: Infinitesimal) = this(x :: y :: Nil)

    override def toReal: Option[Real] = items.map(i => i.toReal).foldLeft(Some(Zero).asInstanceOf[Option[Real]])((l, r) => r.map(_ + l.get))

    override def toString = items.mkString(" + ")

}

class NegatedInfinitesimal(of: Infinitesimal)
        extends Infinitesimal {

    override def unary_-() = of

    override def toReal = of.toReal.map(-_)

    override def toString = "-" + of.toString

}

class InverseInfinitesimal(of: Infinitesimal)
        extends Massive {

    override def inverse = of

    override def closestReal = of.closestReal.inverse

    override def toString = "1/" + of

}