package net.ollie.maths

/**
 * Created by Ollie on 26/02/14.
 * @see http://mathworld.wolfram.com/Exponentiation.html
 */
trait Exponentiated
        extends Expression {

    def base: Expression

    def power: Expression

    def isEmpty = base.isEmpty

    override def toString = s"($base ^ $power)"

}
