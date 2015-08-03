package net.ollie.maths.tensors.vectors

import net.ollie.maths.numbers.Natural

/**
 * Created by Ollie on 09/03/14.
 */
trait VectorBasis {

    type Magnitude <: Natural

    def magnitude: Magnitude

    def toVector: Vector[this.type]

    def toCovector: Covector[this.type]

}

