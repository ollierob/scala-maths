package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.functions.numeric.SquareRoots
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.complex.Complex

trait QuadraticPolynomial
    extends Polynomial {

    override val degree = 2

}

object QuadraticPolynomial {

    def is(poly: Polynomial) = poly.degree == 2

    def apply(x: Variable, a: Complex, b: Complex, c: Complex): SingleVariablePolynomial = {
        if (a.isZero) LinearPolynomial(x, b, c)
        new SingleVariableQuadraticPolynomial(x, a, b, c)
    }

}

/**
 * Polynominal of the form `a.x^2 + b.x + c` with real constants.
 *
 * @param x variable
 * @param a multiplier of variable-squared
 * @param b multiplier of variablePolynomial
 * @param c constant
 */
class SingleVariableQuadraticPolynomial(val x: Variable, val a: Complex, val b: Complex, val c: Complex)
    extends QuadraticPolynomial with SingleVariablePolynomial {

    require(!a.isZero)

    override def of = x;

    override def representation = (a * x ^ 2) + (b * x) + c

    override def isEmpty = false

    override def roots = new QuadraticRoots(this)

    override def toString = s"$a.x^2 + $b.x + $c"

    private class QuadraticRoots(val of: SingleVariableQuadraticPolynomial)
        extends PolynomialRoots[Complex, Complex] {

        lazy val bSquared: Complex = of.b ^ 2
        lazy val roots: Array[Complex] = SquareRoots(bSquared - (4 * of.a * of.c)).values.toArray
        lazy val principal = (-of.b + roots.apply(0)) / (2 * of.a)
        lazy val secondary = (-of.b + roots.apply(1)) / (2 * of.a)

        override def isEmpty = roots.isEmpty

        override def values = {
            if (isEmpty) Set()
            else Set(principal, secondary)
        }

    }

}