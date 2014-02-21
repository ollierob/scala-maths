package net.ollie.maths.functions

import net.ollie.maths.{Expression, Variable}

/**
 * Inherited by any class that is a wrapper for a more complicated representation, degree.
 * Created by Ollie on 10/01/14.
 */
trait Represented
        extends Expression {

    def representation: Expression

    def variables = representation.variables

    def replace(variables: Map[Variable, Expression]) = representation.replace(variables)

    def toConstant = representation.toConstant

    def isEmpty = representation.isEmpty

    def df(x: Variable) = representation.df(x)

    def unary_-(): Expression = Expression.negate(this)

}
