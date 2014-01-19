package net.ollie.maths.functions

import net.ollie.maths.{Expression, Variable}

/**
 * Inherited by any class that is a wrapper for a more complicated representation, f.
 * Created by Ollie on 10/01/14.
 */
trait Represented
        extends Expression {

    protected[this] def f: Expression

    def variables = f.variables

    def replace(variables: Map[Variable, Expression]) = f.replace(variables)

    def toConstant = f.toConstant

    def isEmpty = f.isEmpty

    def df(x: Variable): Expression = f.df(x)

}