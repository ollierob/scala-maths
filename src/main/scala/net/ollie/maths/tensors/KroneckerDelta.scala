package net.ollie.maths.tensors

import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.{One, Zero}

/**
 * Created by Ollie on 09/03/14.
 */
object KroneckerDelta
    extends ConstantTensor[One.type, One.type] {

    def valence = (One, One)

    def apply(i: Natural, j: Natural): Natural = if (i == j) One else Zero

}
