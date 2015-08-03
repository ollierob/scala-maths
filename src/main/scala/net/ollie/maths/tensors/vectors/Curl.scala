package net.ollie.maths.tensors.vectors

import net.ollie.maths.tensors.vectors

/**
 * Created by Ollie on 08/03/14.
 * @see [[Gradient]]
 * @see [[Divergence]]
 */
object Curl {

    def apply[B <: VectorBasis](v: vectors.Vector[B]): vectors.Vector[B] = v match {
        //case c: Curlable[B] => c.curl
        case _ => ???
    }

}

trait Curlable[B <: VectorBasis] {

    this: Vector[B] =>

    def curl: vectors.Vector[B]

}
