package net.ollie.maths.functions.polynomial

import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.Variable
import net.ollie.maths.expressions.Expression

/**
 * @see [[LinearPolynomial]]
 * @see [[QuadraticPolynomial]]
 */
trait NthDegreePolynomial
    extends Polynomial {

}

object NthDegreePolynomial {

    def apply(x: Variable, coefficients: Array[Complex]): UnivariatePolynomial = coefficients.length match {
        case 0 => Polynomial.apply(x)
        case 1 => Polynomial.apply(x, coefficients.apply(0))
        case 2 => Polynomial.apply(x, coefficients.apply(1), coefficients.apply(0))
        case 3 => Polynomial.apply(x, coefficients.apply(2), coefficients.apply(1), coefficients.apply(0))
        case _ => new UnivariateNthDegreePolynomial(x, coefficients)
    }

}

private class UnivariateNthDegreePolynomial(val x: Variable, val coefficients: Array[Complex])
    extends NthDegreePolynomial with UnivariatePolynomial {

    //Should delegate to other types
    require(coefficients.length > 3)

    override def of = x

    override def degree = coefficients.length

    override lazy val representation = {
        var expr: Expression = 0
        for (i <- coefficients.indices) {
            val coeff = coefficients.apply(i)
            if (!coeff.isZero) {
                if (i == 0) expr += coeff
                else expr += coeff * (x ^ i)
            }
        }
        expr;
    }

    override lazy val roots = new NthDegreeRoots(this)

    class NthDegreeRoots(val of: UnivariateNthDegreePolynomial)
        extends PolynomialRoots[Complex, Complex] {

        override def principal = ???

        override def values = ???

    }

    override def df = ??? //TODO

}
