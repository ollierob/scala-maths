package net.ollie.maths

import net.ollie.maths.numbers.PositiveReal
import net.ollie.maths.numbers.constants.Zero

/**
 * An empty expression.
 *
 * For a number this means that it is known to be equal to zero at compile-time.
 * It may not be possible to know if a number is zero at compile-time;
 * the method isEmpty should be called, rather than pattern-matching on this trait.
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

    def toConstant: Option[Constant with Empty] = Some(Zero)

    override def unary_-(): Empty = Zero

    override def ?+(that: Expression)(leftToRight: Boolean): Option[Expression] = Some(that)

    override def ?*(that: Expression)(leftToRight: Boolean): Option[Expression] = Some(this)

    override def df(x: Variable): Empty

    override def hashCode = 0

    override def toString = "0"

}


