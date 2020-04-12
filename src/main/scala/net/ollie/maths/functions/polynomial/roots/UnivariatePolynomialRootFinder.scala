package net.ollie.maths.functions.polynomial.roots

import net.ollie.maths.functions.polynomial.UnivariatePolynomial
import net.ollie.maths.numbers.complex.Complex

trait UnivariatePolynomialRootFinder {

    def roots(polynomial: UnivariatePolynomial): Iterator[Complex]

}

trait NumericalUnivariatePolynomialRootFinder
    extends UnivariatePolynomialRootFinder