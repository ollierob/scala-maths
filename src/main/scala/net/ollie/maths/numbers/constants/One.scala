package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.Natural.FactorialCache
import net.ollie.maths.numbers._
import net.ollie.maths.{Constant, Expression}

/**
 * Created by Ollie on 09/02/14.
 * @see http://mathworld.wolfram.com/Unity.html
 */
trait Unity
        extends Constant {

    def abs = One

}

object One
        extends ExactNatural(1)
        with Unity {

    override def !(implicit cache: FactorialCache) = this

    override def abs = super[Unity].abs

    override def unary_-() = MinusOne

    override def decr = Zero

    override def *(that: Real) = that

    override def *(that: PositiveReal) = that

    override def *(that: Natural) = that

    override def ?*(that: Expression)(leftToRight: Boolean) = Some(that)

    override def ?*(that: Constant)(leftToRight: Boolean): Option[Constant] = Some(that)

    override def ?*(that: Real) = Some(that)

    override def ?*?(that: Real) = Some(that)

    override def ^(that: Expression) = this

    override def ^(that: Integer) = this

    override def ^(that: Natural) = this

    override def ?^(that: Constant) = Some(this)

}

object MinusOne
        extends ExactInteger(-1)
        with Unity {

    override def unary_-() = One

    override def ^(that: Integer): Integer = if (that.isEven) One else this

    override def abs = super[Unity].abs

    override def equals(re: Real) = (this eq re) || super.equals(re)

}

object Two
        extends ExactNatural(2)
        with Prime {

    override def !(implicit cache: FactorialCache) = this

    override def isEven = true

}

object Three
        extends ExactNatural(3)
        with Prime