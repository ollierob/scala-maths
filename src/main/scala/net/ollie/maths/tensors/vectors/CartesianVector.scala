package net.ollie.maths.tensors.vectors

import net.ollie.maths.Constant
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.{One, Three}

/**
 * Created by Ollie on 09/03/14.
 */
trait CartesianVector[B <: CartesianVectorBasis]
        extends Vector[B]

class ConstantCartesianVector[B <: CartesianVectorBasis, E <: Constant](val basis: B, val terms: Seq[E])
        extends CartesianVector[B]
        with ConstantVector[B] {

    type Element = E

}

trait CartesianVectorBasis
        extends VectorBasis {

    def toVector: CartesianVector[this.type]

}

object CartesianThreeVectorBasis
        extends CartesianVectorBasis {

    type Magnitude = Three.type

    def magnitude = Three

    def toVector = new ConstantCartesianVector(this, Seq[Real](One, One))

    def toCovector = ???

}

