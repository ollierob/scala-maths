package net.ollie.maths.tensors.vectors

import net.ollie.maths.expressions.Expression

/**
 * Created by Ollie on 08/03/14.
 *
 * @see [[Divergence]]
 * @see [[Curl]]
 */
object Gradient {

    def apply[B <: VectorBasis](v: Expression)(implicit basis: B): Covector[B] = {
        v match {
            case g: Gradable => g.grad(basis)
            case _ => ???
        }
    }

}

trait Gradable {

    def grad[B <: VectorBasis](basis: B): Covector[B]

}
