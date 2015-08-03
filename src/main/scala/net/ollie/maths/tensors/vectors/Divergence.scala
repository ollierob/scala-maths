package net.ollie.maths.tensors.vectors

import net.ollie.maths.Expression
import net.ollie.maths.tensors.vectors

/**
 * Created by Ollie on 08/03/14.
 * @see [[Gradient]]
 * @see [[Curl]]
 */
object Divergence {

    def apply(v: vectors.Vector[_]): Expression = v match {
        case d: Divable[_] => d.div
        case _ => ???
    }

}

trait Divable[B <: VectorBasis] {

    this: Vector[B] =>

    def div: Expression

}
