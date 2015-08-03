package net.ollie.maths.numbers

import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.{Constant, Empty, Evaluable, Expression}

/**
 * Created by Ollie on 02/03/14.
 */
trait EmptyConstant
        extends Empty
        with Constant
        with Evaluable
        with MaybeReal {

    private val ZERO = BigDecimal(0)

    override def narrow: System with EmptyConstant = this

    def abs: PositiveReal with EmptyConstant = Zero

    override def unary_-(): EmptyConstant with System = this

    override def variables = super[Constant].variables

    override def isEmpty = super[Empty].isEmpty

    def evaluate(precision: Precision) = ZERO to precision

    override def toConstant: Option[System with EmptyConstant] = Some(this.narrow)

    override def ?*(that: Expression)(leftToRight: Boolean) = super[Empty].?*(that)(leftToRight)

    override def toString = super[Empty].toString

}
