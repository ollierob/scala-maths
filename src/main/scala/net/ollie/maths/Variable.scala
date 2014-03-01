package net.ollie.maths

import net.ollie.maths.numbers.constants.{Zero, One}
import java.util.UUID

/**
 * Created by Ollie on 02/01/14.
 */
object Variable {

    def apply(name: String): Variable = new NamedVariable(name)

    def virtual: Variable = Variable('$' + UUID.randomUUID.toString)

}

trait Variable
        extends Univariate
        with Integrable {

    def variable = this

    def name: String

    def isEmpty = false

    def toConstant = None

    def replace(variables: Map[Variable, Expression]) = variables.get(this) match {
        case Some(expr) => expr
        case _ => this
    }

    def df(x: Variable): Univariate = {
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

    def unary_-() = Expression.negate(this)

    override def equals(expression: Expression) = expression match {
        case variable: Variable => this.name == variable.name
        case _ => super.equals(expression)
    }

    override def hashCode = name.hashCode

}
