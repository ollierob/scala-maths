package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.Real

/**
 * Created by Ollie on 26/01/14.
 * @see http://mathworld.wolfram.com/Minimum.html
 */
object Min {

    def apply[N <: Ordered[M], M >: N](values: N*)(implicit cmp: Ordering[M]): N = values.min(cmp)

    def apply[N <: Real](values: N*): N = values.min(Real.RealArithmetic)

}
