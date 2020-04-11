package net.ollie.maths.expressions

import net.ollie.maths.Variable

/**
 * Multiplication of two expressions. If either expression is empty then so is their multiplication.
 *
 * Commutativity is not assumed, nor asked for, as it cannot be known for non-constant expressions.
 *
 * Created by Ollie on 08/03/14.
 */
trait Multiplied
        extends Expression {

    def left: Expression

    def right: Expression

    override def df(x: Variable): Expression = {
        (left.df(x) * right) + (left * right.df(x))
    }

    override def isEmpty = left.isEmpty || right.isEmpty

    override def toString = s"($left * $right)"

}
