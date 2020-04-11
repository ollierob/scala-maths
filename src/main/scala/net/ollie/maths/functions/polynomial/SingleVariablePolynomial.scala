package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable

trait SingleVariablePolynomial
    extends Polynomial {

    override def of: Variable

    override def variables = Set(of)

    def roots: PolynomialRoots[_, _]

}

object SingleVariablePolynomial {

    def is(poly: Polynomial) = poly.variables.size == 1

}
