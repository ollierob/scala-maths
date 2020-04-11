package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.expressions.{Expression, Univariate}

trait UnivariatePolynomial
    extends Polynomial with Univariate {

    override def of: Variable

    override def variables = Set(of)

    override def replace(variables: Map[Variable, Expression]) = {
        if (variables.contains(of)) representation.replace(variables)
        else this
    }

    def roots: PolynomialRoots[_, _]

}

object UnivariatePolynomial {

    def is(poly: Polynomial) = poly.variables.size == 1

}