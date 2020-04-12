package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.numeric.SquareRoots
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{One, Two, Zero}

/**
 * @see [[NthDegreePolynomial]]
 */
trait QuadraticPolynomial
    extends Polynomial {

    override val degree = 2

    //def df(x: Variable): LinearPolynomial

}

object QuadraticPolynomial {

    def is(poly: Polynomial) = poly.degree == 2

    def apply(x: Variable, a: Complex, b: Complex, c: Complex): UnivariatePolynomial = {
        if (a.isZero) LinearPolynomial(x, b, c)
        new UnivariateQuadraticPolynomial(x, a, b, c)
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
class UnivariateQuadraticPolynomial(val x: Variable, val a: Complex, val b: Complex, val c: Complex)
    extends QuadraticPolynomial with UnivariatePolynomial {

    require(!a.isZero)

    override def of = x;

    override def replace(variables: Map[Variable, Expression]) = {
        variables.get(x).map(e => (a * (e ^ 2)) + (b * e) + c).getOrElse(this)
    }

    override def isEmpty = false

    override def derivative = Polynomial(x, 2 * a, b)

    override def roots = new QuadraticRoots(this)

    override def coefficient(power: Natural): Complex = power match {
        case Two => a
        case One => b
        case Zero => c
        case _ => 0
    }

    override def equals(exp: Expression) = exp match {
        case p: UnivariateQuadraticPolynomial => x == p.x && a == p.a && b == p.b && c == p.c
        case _ => super.equals(exp)
    }

    override def toString = s"($a.x^2 + $b.x + $c)"

    class QuadraticRoots(val of: UnivariateQuadraticPolynomial)
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