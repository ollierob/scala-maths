package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.numbers.Natural
import net.ollie.maths.{Constant, Variable}

trait UnivariatePolynomial
    extends Polynomial with Univariate {

    override def of: Variable

    override def variable = of

    override def variables = Set(of)

    def coefficient(power: Natural): Constant

    override def replace(variables: Map[Variable, Expression]): Expression = {
        if (variables.contains(of)) representation.replace(variables)
        else this
    }

    def replace(c: Constant): Constant = {
        replace(Map(of -> c)).toConstant.get
    }

    def roots: PolynomialRoots[_, _]

    override def unary_-(): UnivariatePolynomial = ??? //TODO

    override def df(x: Variable): UnivariatePolynomial = {
        if (x == variable) derivative
        else Polynomial(x)
    }

    def derivative: UnivariatePolynomial

}

object UnivariatePolynomial {

    def is(poly: Polynomial) = poly.variables.size == 1

}