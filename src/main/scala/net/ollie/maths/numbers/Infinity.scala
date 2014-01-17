package net.ollie.maths.numbers


import net.ollie.maths.Empty

/**
 * Basic implementations of infinity.
 * Created by Ollie on 05/01/14.
 * @see http://mathworld.wolfram.com/AffinelyExtendedRealNumbers.html
 */
trait Infinite
        extends RealNumber {

    def isEmpty = false

    protected[this] def eval(precision: Precision): BigDecimal = ???

    override def abs: PositiveRealNumber with Infinite = Infinity

    override def inverse: RealNumber with Empty = Zero

    override def ?+(that: RealNumber) = that match {
        case i: Infinite if i == -this => ???
        case _ => Some(this)
    }

    override def ?*(that: RealNumber) = that match {
        case Zero => ???
        case _ if (that > 0) => Some(this)
        case _ => Some(-this)
    }

    override def tryCompareTo(that: RealNumber) = Some(this.compareTo(that))

    override def ?==(that: RealNumber) = Some(this eq that)

}

object Infinity
        extends PositiveRealNumber
        with Infinite {

    override def abs = this

    override def unary_-() = NegativeInfinity

    override def inverse = Zero

    override def compareTo(that: RealNumber) = 1

    override def isStrictlyPositive = true

    override def toString = "∞"

}

object NegativeInfinity
        extends Infinite {

    override def abs = Infinity

    override def unary_-() = Infinity

    override def compareTo(that: RealNumber) = -1

    override def isStrictlyPositive = false

    override def toString = "-∞"

}

object UnsignedInfinity
        extends PositiveRealNumber
        with Infinite {

    override def unary_-() = this

    override def inverse = Zero

    override def ?+(that: RealNumber) = that match {
        case i: Infinite => ???
        case _ => Some(this)
    }

    override def ?*(that: RealNumber) = that match {
        case i: Infinite => ???
        case Zero => ???
        case _ => Some(this)
    }

    override def toString = "±∞"

}