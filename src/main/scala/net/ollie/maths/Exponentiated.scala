package net.ollie.maths

import net.ollie.maths.functions.numeric.Ln

/**
 * Created by Ollie on 26/02/14.
 * @see http://mathworld.wolfram.com/Exponentiation.html
 */
trait Exponentiated
        extends Expression {

    def base: Expression

    def power: Expression

    override def isEmpty = base.isEmpty

    override def df(x: Variable): Expression = {
        (base ^ (power - 1)) * ((base.df(x) * power) + (base * Ln(base) * power.df(x)))
    }

    override def toString = s"($base ^ $power)"

}
