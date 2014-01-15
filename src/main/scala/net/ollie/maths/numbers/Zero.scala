package net.ollie.maths.numbers

import net.ollie.maths.{Variable, Empty}

/**
 * Empty real number.
 * Created by Ollie on 01/01/14.
 */
object Zero
        extends NaturalNumber
        with Empty {

    private lazy val evaluated: BigInt = 0

    def evaluate = evaluated

    override def variables = super[Empty].variables

    override def isEmpty = super[Empty].isEmpty

    override def inverse = UnsignedInfinity

    override def unary_-() = this

    override def succ = One

    override def ! = One

    override def df(x: Variable) = this

    override def ?+(that: RealNumber) = Some(that)

    override def +(that: IntegerNumber) = that

    override def +(that: NaturalNumber) = that

    override def ?*(that: RealNumber) = Some(this)

    override def *(that: IntegerNumber) = this

    override def *(that: NaturalNumber) = this

}
