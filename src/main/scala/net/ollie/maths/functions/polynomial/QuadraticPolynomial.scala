package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.functions.numeric.SquareRoot
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.One
import net.ollie.maths.numbers.{Natural, Real}

trait QuadraticPolynomial
    extends Polynomial {

}

object QuadraticPolynomial {

    def is(poly: Polynomial) = poly.degree == 2

    def apply(x: Variable, a: Real, b: Real, c: Real) = new SingleVariableRealQuadratic(x, a, b, c)

}

/**
 * Polynominal of the form `a.x^2 + b.x + c` with real constants.
 *
 * @param x variable
 * @param a multiplier of variable-squared
 * @param b multiplier of variable
 * @param c constant
 */
class SingleVariableRealQuadratic(val x: Variable, val a: Real, val b: Real, val c: Real)
    extends QuadraticPolynomial with SingleVariablePolynomial {

    require(a != 0)

    override def degree: Natural = {
        if (!a.isZero) return 2;
        if (!b.isZero) return One;
        0;
    }

    override def of = x;

    override def representation = (a * x ^ 2) + (b * x) + c

    override def isEmpty = a.isEmpty && b.isEmpty && c.isEmpty

    override def roots = new QuadraticRoots(this)

    override def toString = s"$a.x^2 + $b.x + $c"

}

class QuadraticRoots(val of: SingleVariableRealQuadratic)
    extends PolynomialRoots[Real, Complex] {

    lazy val bSquared: Real = of.b ^ 2
    lazy val roots: Array[Complex] = SquareRoot(bSquared - (4 * of.a * of.c)).values.toArray
    lazy val principal = (-of.b + roots.apply(0)) / (2 * of.a)
    lazy val secondary = (-of.b + roots.apply(1)) / (2 * of.a)

    override def isEmpty = roots.isEmpty

    override def values = {
        if (isEmpty) Set()
        else Set(principal, secondary)
    }

}