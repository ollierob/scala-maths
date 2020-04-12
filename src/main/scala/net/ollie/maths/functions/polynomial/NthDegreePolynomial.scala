package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Constant, Variable}
import net.ollie.maths.expressions.Expression
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.complex.Complex

/**
 * @see [[LinearPolynomial]]
 * @see [[QuadraticPolynomial]]
 */
trait NthDegreePolynomial
    extends Polynomial {

}

object NthDegreePolynomial {

    def apply(x: Variable, coefficients: Seq[Complex]): UnivariatePolynomial = degree(coefficients) match {
        case 0 => Polynomial(x, coefficients.apply(0))
        case 1 => Polynomial(x, coefficients.apply(1), coefficients.apply(0))
        case 2 => Polynomial(x, coefficients.apply(2), coefficients.apply(1), coefficients.apply(0))
        case _ => new UnivariateNthDegreePolynomial(x, coefficients)
    }

    def degree(coefficients: Seq[Complex]): Int = coefficients.lastIndexWhere(c => !c.isZero)

}

private class UnivariateNthDegreePolynomial(val x: Variable, val coefficients: Seq[Complex])
    extends NthDegreePolynomial with UnivariatePolynomial {

    //Should delegate to other types
    require(coefficients.length > 3)

    override val of = x

    override val degree = NthDegreePolynomial.degree(coefficients)

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

    override def coefficient(power: Natural): Complex = {
        if (power < coefficients.length) coefficients.apply(power.requireInt)
        else 0
    }

    class NthDegreeRoots(val of: UnivariateNthDegreePolynomial)
        extends PolynomialRoots[Complex, Complex] {

        override def principal = ???

        override def values = ???

    }

    override def derivative: UnivariatePolynomial = {
        val newCoefficients = coefficients.slice(1, coefficients.length).zipWithIndex.map(t => t._1 * (t._2 + 1))
        Polynomial(x, newCoefficients);
    }

}
