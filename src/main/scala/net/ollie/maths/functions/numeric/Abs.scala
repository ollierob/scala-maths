package net.ollie.maths.functions.numeric

import net.ollie.maths.Number
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers.PositiveReal

/**
 * Created by Ollie on 10/01/14.
 */
object Abs
        extends UnivariateFunction[Number, PositiveReal] {

    def apply(n: Number) = n.abs

    override def toString = "|?|"

}
