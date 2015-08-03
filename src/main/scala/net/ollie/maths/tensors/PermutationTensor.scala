package net.ollie.maths.tensors

import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.{Three, Zero}

/**
 * Created by Ollie on 09/03/14.
 */
object PermutationTensor
        extends CovariantTensor[Three.type] {

    type Element = Real

    def valence = (Three, Zero)

    def toConstant: Option[ConstantTensor[Three.type, Zero.type]] = ???

}
