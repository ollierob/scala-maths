package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers._
import net.ollie.maths.Number
import scala.Some

/**
 * Created by Ollie on 09/02/14.
 */
object One
        extends ExactNatural(1) {

    override def ! = this

    override def unary_-() = MinusOne

    override def decr = Zero

    override def ?*(that: Number)(leftToRight: Boolean): Option[Number] = Some(that)

    override def *(that: Natural) = that

    override def ?*(that: Real) = Some(that)

}

object MinusOne
        extends ExactInteger(-1) {

    override def unary_-() = One

    override def ^(that: Integer): Integer = if (that.isEven) One else this

    override def equals(re: Real) = (this eq re) || super.equals(re)

}