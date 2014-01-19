package net.ollie.maths

import net.ollie.maths.numbers.Zero

/**
 * An empty expression.
 *
 * For a number this means that it is known to be equal to zero at compile-time.
 * It may not be possible to know if a number is zero at compile-time;
 * the method [[net.ollie.maths.Empty.isEmpty]] should be called, rather than pattern-matching on this trait.
 *
 * For an expression, it means that it evaluates to zero everywhere.
 *
 * Created by Ollie on 02/01/14.
 * @see Zero
 */
trait Empty
        extends Expression {

    def isEmpty = true

    def variables = Set.empty

    override def unary_-(): Empty = Zero

    override def +(that: Expression) = that

    override def *(that: Expression) = Zero

    override def toString = "0"

}
