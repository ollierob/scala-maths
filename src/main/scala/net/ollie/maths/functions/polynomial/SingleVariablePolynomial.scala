package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.expressions.Expression

trait SingleVariablePolynomial
    extends Polynomial {

    override def of: Variable

    override def variables = Set(of)

    override def replace(variables: Map[Variable, Expression]) = {
        if (variables.contains(of)) representation.replace(variables)
        else this
    }

    def roots: PolynomialRoots[_, _]

}

object SingleVariablePolynomial {

    def is(poly: Polynomial) = poly.variables.size == 1

}