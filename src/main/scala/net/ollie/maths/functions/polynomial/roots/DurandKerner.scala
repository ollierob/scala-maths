package net.ollie.maths.functions.polynomial.roots

import net.ollie.maths.functions.polynomial.UnivariatePolynomial
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.{IntegerPrecision, Precision}

import scala.collection.mutable.ArrayBuffer

/**
 * @see [[https://en.wikipedia.org/wiki/Durand-Kerner_method]]
 */
class DurandKerner(val initial: Complex)
    extends NumericalUnivariatePolynomialRootFinder {

    def roots(polynomial: UnivariatePolynomial) = {
        new Solver(polynomial).roots.iterator
    }

    private class Solver(polynomial: UnivariatePolynomial) {

        require(polynomial.degree.toInt.isDefined, () => "Cannot solve for high/infinite degree polynomials: " + polynomial)

        private var precision: Precision = IntegerPrecision

        def roots: List[Complex] = List.tabulate(polynomial.degree.requireInt)(i => new Solution)

        def next(current: List[Complex]): List[Complex] = {
            val next = new ArrayBuffer[Complex](current.length)
            for (i <- current.indices) {
                val z = current.apply(i)
                next += z - (f(z) / denominator(i, current))
            }
            next.toList
        }

        def f(z: Complex): Complex = {
            Complex(polynomial.replace(z)).get
        }

        def denominator(index: Int, coefficients: List[Complex]): Complex = {
            val pn = coefficients.apply(index);
            var product: Complex = 1
            for (i <- coefficients.indices) {
                if (i != index) {
                    product *= (pn - coefficients.apply(i))
                }
            }
            product
        }

    }

    private class Solution extends Complex {

        private var current: Complex = initial

        override def re = current.re

        override def im = current.im

    }

}
