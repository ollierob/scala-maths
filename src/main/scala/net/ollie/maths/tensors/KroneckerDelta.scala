package net.ollie.maths.tensors

import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 09/03/14.
 */
object KroneckerDelta
        extends ConstantTensor[One.type, One.type] {

    def valence = (One, One)

}
