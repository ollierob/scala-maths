package net.ollie.maths

import net.ollie.maths.numbers.Zero

/**
 * Created by Ollie on 02/01/14.
 */
trait Empty
        extends Differentiable {

    def isEmpty = true

    def variables = Set.empty

    override def unary_-(): Empty = Zero

    override def +(that: Expression) = that

    override def +(that: Differentiable) = that

    override def *(that: Expression) = Zero

    override def *(that: Differentiable) = Zero

    override def toString = "0"

}
