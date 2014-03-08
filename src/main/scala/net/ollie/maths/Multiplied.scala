package net.ollie.maths

/**
 * Multiplication of two expressions.
 *
 * Commutativity is not assumed, nor asked for, as it cannot be known for non-constant expressions.
 *
 * Created by Ollie on 08/03/14.
 */
trait Multiplied
        extends Expression {

    def left: Expression

    def right: Expression

    override def isEmpty = left.isEmpty || right.isEmpty

    override def toString = s"($left * $right)"

}
