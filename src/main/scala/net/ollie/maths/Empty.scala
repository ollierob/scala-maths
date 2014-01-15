package net.ollie.maths

import net.ollie.maths.numbers.Zero

/**
 * An empty expression.
 * For a number, it may mean that it is equal to zero, though this may not be known until it is evaluated.
 * Created by Ollie on 02/01/14.
 * @see Zero
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
