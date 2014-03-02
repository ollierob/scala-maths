package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.Real

/**
 * Created by Ollie on 12/01/14.
 */
class CartesianComplex(val re: Real, val im: Real)
        extends Complex {

    override def toString = "(" + re.toString + " + " + im.toString + "i)"

}