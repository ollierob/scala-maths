package net.ollie.maths

import net.ollie.maths.numbers.{Precision, Zero}

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

    override def unary_-(): Empty = Zero

    override def +(that: Expression) = that

    override def ?*(that: Expression)(leftToRight: Boolean) = Some(this)

    override def toString = "0"

}

trait EmptyNumber
        extends Empty
        with Number
        with Evaluable {

    override def unary_-() = this

    override def variables = super[Number].variables

    override def isEmpty = super[Empty].isEmpty

    override def toString = super[Empty].toString

    protected def eval(precision: Precision) = BigDecimal(0) to precision

    override def ?*(that: Expression)(leftToRight: Boolean) = super[Empty].?*(that)(leftToRight)

}
