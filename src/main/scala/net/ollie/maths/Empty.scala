package net.ollie.maths

import net.ollie.maths.numbers.{PositiveReal, Precision}
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

    def toConstant: Option[Constant] = Some(Zero)

    override def unary_-(): Empty = Zero

    override def ?+(that: Expression)(leftToRight: Boolean): Option[Expression] = Some(that)

    override def ?*(that: Expression)(leftToRight: Boolean): Option[Expression] = Some(this)

    override def df(x: Variable): Empty

    override def toString = "0"

}

trait EmptyConstant
        extends Empty
        with Constant
        with Evaluable {

    private val ZERO = BigDecimal(0)

    def abs: PositiveReal with EmptyConstant = Zero

    override def unary_-(): EmptyConstant with System = this

    override def variables = super[Constant].variables

    override def isEmpty = super[Empty].isEmpty

    override def toString = super[Empty].toString

    def evaluate(precision: Precision) = ZERO to precision

    override def toConstant: Option[System] = Some(this.narrow)

    override def ?*(that: Expression)(leftToRight: Boolean) = super[Empty].?*(that)(leftToRight)

}
