package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.Natural.FactorialCache
import net.ollie.maths.numbers._
import net.ollie.maths.{Expression, Operation, Variable}

/**
 * Empty real number.
 * Created by Ollie on 01/01/14.
 */
object Zero
        extends Natural
        with EmptyConstant {

    private final val i = BigInt(0)

    def evaluate = i

    override def evaluate(precision: Precision) = super[EmptyConstant].evaluate(precision)

    override def isEmpty = super[EmptyConstant].isEmpty

    override def isEven = true

    override def inverse: PositiveReal with Rational = Operation.undefined //UnsignedInfinity //TODO

    override def unary_- = this

    override def abs = this

    override def succ = One

    override def !(implicit cache: FactorialCache) = One

    override def df(x: Variable) = this

    override def ?+(that: Real) = Some(that)

    override def +(that: Natural) = that

    override def ?*(that: Real) = Some(this)

    override def *(that: Natural) = this

    override def replace(variables: Map[Variable, Expression]) = this

    override def ~=(that: Real)(implicit precision: Precision) = that.isEmpty || super.~=(that)

    override def toString = super[EmptyConstant].toString

}
