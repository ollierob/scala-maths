package net.ollie.maths

import java.util.UUID

import net.ollie.maths.expressions.{Expression, Integrable, Linear, Univariate}
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.{One, Zero}

/**
 * Created by Ollie on 02/01/14.
 */
object Variable {

    def apply(name: String): Variable = new NamedVariable(name)

    def virtual: Variable = Variable("$" + UUID.randomUUID.toString)

}

trait Variable
        extends Linear
        with Integrable {

    def left: Real = One

    def right: Real = One

    def variable = this

    def name: String

    override def toConstant = None

    override def isEmpty = false

    override def ?*(that: Expression)(leftToRight: Boolean): Option[Expression] = that.toConstant match {
        case Some(n) => Some(Linear.multiply(this, n)(leftToRight))
        case _ => super.?*(that)(leftToRight)
    }

    override def ?/(that: Expression): Option[Expression] = {
        that.toConstant match {
            case Some(n) => Some(Linear.divide(this, n))
            case _ => super.?/(that)
        }
    }

    def replace(variables: Map[Variable, Expression]) = variables.get(this) match {
        case Some(expr) => expr
        case _ => this
    }

    override def df(x: Variable): Univariate = {
        if (this == x) One
        else Zero
    }

    override protected[this] def integral(x: Variable): Expression = {
        if (this == x) (this ^ 2) / 2
        else Zero
    }

    override def toString = name

}

private class NamedVariable(val name: String)
        extends AnyRef
        with Variable {

    override def equals(expression: Expression) = expression match {
        case variable: Variable => this.name == variable.name
        case _ => super.equals(expression)
    }

    override def hashCode = name.hashCode

}
