package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.{PositiveReal, Real}

/**
 * Created by Ollie on 02/03/14.
 */
trait NamedReal
        extends Real {

    override def isEmpty = false

    override def equals(that: Real) = this.eq(that) || super.equals(that)

}

trait PositiveNamedReal
        extends NamedReal
        with PositiveReal