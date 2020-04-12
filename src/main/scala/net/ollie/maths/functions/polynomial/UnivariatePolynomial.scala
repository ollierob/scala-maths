package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Constant, Variable}
import net.ollie.maths.expressions.{Expression, Univariate}

trait UnivariatePolynomial
    extends Polynomial with Univariate {

    override def of: Variable

    override def variable = of

    override def variables = Set(of)

    override def replace(variables: Map[Variable, Expression]) = {
        if (variables.contains(of)) representation.replace(variables)
        else this
    }

    def replace(c: Constant): Constant = {
        val replaced = replace(Map(of -> c))
        ???
    }

    def roots: PolynomialRoots[_, _]

    override def unary_-(): UnivariatePolynomial = ??? //TODO

    override def df(x: Variable): UnivariatePolynomial = {
        if(x == variable) derivative
        else this
    }

    def derivative: UnivariatePolynomial

}

object UnivariatePolynomial {

    def is(poly: Polynomial) = poly.variables.size == 1

}