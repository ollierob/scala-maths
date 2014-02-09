package net.ollie.maths.methods

import net.ollie.maths.{Aggregate, Expression, Variable}
import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 27/01/14.
 */
object Power {

    def apply(base: Expression, power: Expression): Expression = if (base.isEmpty) Zero else new Power(base, power)

}

class Power(val base: Expression, val power: Expression)
        extends Aggregate {

    protected def terms = Seq(base, power)

    override def replace(variables: Map[Variable, Expression]) = Power(base.replace(variables), power.replace(variables))

    protected[this] def apply(expressions: Seq[Expression]) = ???

    def toConstant = base.toConstant match {
        case Some(b) => power.toConstant match {
            case Some(p) => b ?^ p
            case _ => None
        }
        case _ => None
    }

    def isEmpty = base.isEmpty

    def df(x: Variable) = (base ^ (power - 1)) * ((base.df(x) * power) + (base * Ln(power))) * power.df(x)

    override def toString = s"($base ^ $power)"

}