package net.ollie.maths.tensors.vectors

import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.tensors.CovariantTensor

/**
 * A covariant vector.
 * Created by Ollie on 09/03/14.
 * @see http://mathworld.wolfram.com/ColumnVector.html
 */
trait Covector[B <: VectorBasis]
        extends CovariantTensor[One.type] {

    def valence = (One, Zero)

    def basis: B

}
