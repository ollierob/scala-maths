package net.ollie.maths.tensors

import net.ollie.maths.Constant
import net.ollie.maths.numbers.Natural

/**
 * Created by Ollie on 08/03/14.
 * @tparam Co Covariant ("downstairs") type
 * @tparam Contra Contravariant ("upstairs") type
 */
trait Tensor[Co <: Natural, Contra <: Natural] {

    def valence: (Co, Contra)

    def toConstant: Option[ConstantTensor[Co, Contra]]

    def +(that: Tensor[Co, Contra]): Tensor[Co, Contra] = Tensor.add(this, that)

    //def ⊗(that:Tensor[Contra, Co]):

}

object Tensor {

    def add[D <: Natural, U <: Natural](left: Tensor[D, U], right: Tensor[D, U]): Tensor[D, U] = {
        new AddedTensor(left, right)
    }

}

trait ConstantTensor[Co <: Natural, Contra <: Natural]
        extends Tensor[Co, Contra] {

    type Element <: Constant

    def toConstant = Some(this)

}

class AddedTensor[D <: Natural, U <: Natural](left: Tensor[D, U], right: Tensor[D, U])
        extends Tensor[D, U] {

    def toConstant: Option[ConstantTensor[D, U]] = left.toConstant match {
        case Some(l) => right.toConstant match {
            case Some(r) => ??? //TODO
            case _ => None
        }
        case _ => None
    }

    def valence: (D, U) = left.valence

}