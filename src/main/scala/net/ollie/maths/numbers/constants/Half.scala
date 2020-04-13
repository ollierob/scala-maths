package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.{IntegerFraction, NaturalFraction}

/**
 * Created by Ollie on 09/02/14.
 */
object Half
    extends NaturalFraction(1, 2) {

    override def unary_-() = MinusHalf

}

object MinusHalf
    extends IntegerFraction(-1, 2) {

    override def unary_-() = Half

}

object Third
    extends NaturalFraction(1, 3)

object Quarter
    extends NaturalFraction(1, 4)
