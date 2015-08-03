package net.ollie.maths.tensors.vectors

import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.tensors.ConstantTensor

/**
 * Created by Ollie on 09/03/14.
 */
trait ConstantVector[B <: VectorBasis]
        extends Vector[B]
        with ConstantTensor[Zero.type, One.type] {

    override def toConstant = Some(this)

}
