package net.ollie.maths.numbers.real

import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.numbers.{PositiveRealNumber, Precision}

/**
 * Created by Ollie on 05/01/14.
 */
object Pi extends PositiveRealNumber {

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode.RoundingMode): BigDecimal = ???

    def isEmpty = false

    override def toString = "π"

}
