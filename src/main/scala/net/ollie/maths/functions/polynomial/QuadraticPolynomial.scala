package net.ollie.maths.functions.polynomial

import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.One
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.{Constant, Variable}

trait QuadraticPolynomial
    extends Polynomial {

}

object QuadraticPolynomial {

    def is(poly: Polynomial) = poly.degree == 2

    def apply(x: Variable, a: Real, b: Real, c: Real) = new SingleVariableQuadratic(x, a, b, c)

}

/**
 * Polynominal of the form `a.x^2 + b.x + c` with real constants.
 *
 * @param x variable
 * @param a multiplier of variable-squared
 * @param b multiplier of variable
 * @param c constant
 */
class SingleVariableQuadratic(val x: Variable, a: Real, b: Real, c: Real)
    extends QuadraticPolynomial with SingleVariablePolynomial {

    override def degree: Natural = {
        if (!a.isZero) return 2;
        if (!b.isZero) return One;
        0;
    }

    override def of = x;

    override def representation = (a * x ^ 2) + (b * x) + c

    override def isEmpty = a.isEmpty && b.isEmpty && c.isEmpty

    override def roots = new QuadraticRoots(this)

}

class QuadraticRoots(val polynomial: SingleVariableQuadratic)
    extends PolynomialRoots[Real, Complex] {

    override def of: Real = ???

    override def principal: Complex = ???

    override def values: Set[Complex] = ???

    override def inverse: Constant = ???
}