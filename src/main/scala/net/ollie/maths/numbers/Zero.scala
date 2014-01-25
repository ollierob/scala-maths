package net.ollie.maths.numbers

import net.ollie.maths.{EmptyNumber, Variable}

/**
 * Empty real number.
 * Created by Ollie on 01/01/14.
 */
object Zero
        extends Natural
        with EmptyNumber {

    private lazy val evaluated: BigInt = 0

    def evaluate = evaluated

    override def isEmpty = super[EmptyNumber].isEmpty

    override def isEven = true

    override def inverse = UnsignedInfinity

    override def unary_-() = this

    override def succ = One

    override def ! = One

    override def df(x: Variable) = this

    override def ?+(that: Real) = Some(that)

    override def +(that: Natural) = that

    override def ?*(that: Real) = Some(this)

    override def *(that: Natural) = this

    override def toString = super[EmptyNumber].toString

}
