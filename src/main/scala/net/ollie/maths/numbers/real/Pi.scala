package net.ollie.maths.numbers.real

import net.ollie.maths.numbers.{PositiveReal, Precision}

/**
 * Created by Ollie on 05/01/14.
 */
object Pi
        extends PositiveReal {

    private val PI_100 = BigDecimal("3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")

    protected[this] def eval(precision: Precision) = {
        if (precision.value < 100) precision(PI_100)
        else ???
    }

    def isEmpty = false

    override def toString = "π"

}
