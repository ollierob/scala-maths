package net.ollie.maths.numbers.constants

import net.ollie.maths.methods.Series
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 18/12/13.
 * @see http://mathworld.wolfram.com/e.html
 */
object EulersNumber
        extends PositiveReal {

    private final val E50 = BigDecimal("2.71828182845904523536028747135266249775724709369995")

    def isEmpty = false

    protected[this] def doEvaluate(precision: Precision) = {
        if (precision.value < 50) precision(E50)
        else Series(nth _, Zero).evaluate(precision)
    }

    private def nth(n: Natural): Real = 1 / (n !)

    override def toString = "e"

}