package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.functions.numeric.PositiveSquareRoot

/**
 * Created by Ollie on 19/02/14.
 * @see http://mathworld.wolfram.com/GoldenRatio.html
 */
object GoldenRatio
        extends Real {

    private val PHI_100 = BigDecimal("1.6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374847540880753868917521266338622235369317931800607667263544333890865959395829056383226613199282903")

    private val representation: Real = (1 + PositiveSquareRoot(5)) / 2

    def isEmpty = false

    protected[this] def doEvaluate(precision: Precision) = {
        if (precision.digits < 100) PHI_100
        else representation.evaluate(precision)
    }

    override def toString = "ϕ"

}
