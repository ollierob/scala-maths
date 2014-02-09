package net.ollie.maths.numbers.constants

import net.ollie.maths.{Operation, Expression, EmptyNumber, Variable}
import net.ollie.maths.numbers._
import scala.Some

/**
 * Empty real number.
 * Created by Ollie on 01/01/14.
 */
object Zero
        extends Natural
        with EmptyNumber {

    private final val i = BigInt(0)

    def evaluate = i

    private final val d = BigDecimal("0")

    def evaluate(precision: Precision) = d to precision

    override def isEmpty = super[EmptyNumber].isEmpty

    override def isEven = true

    override def inverse: PositiveReal with Rational = Operation.undefined //UnsignedInfinity //TODO

    override def unary_-() = this

    override def abs = this

    override def succ = One

    override def ! = One

    override def df(x: Variable) = this

    override def ?+(that: Real) = Some(that)

    override def +(that: Natural) = that

    override def ?*(that: Real) = Some(this)

    override def *(that: Natural) = this

    override def doEvaluate(precision: Precision) = super[EmptyNumber].doEvaluate(precision)

    override def replace(variables: Map[Variable, Expression]) = this

    override def toString = super[EmptyNumber].toString

}