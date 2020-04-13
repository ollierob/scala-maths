package net.ollie.maths.functions.polynomial.roots

import net.ollie.maths.functions.polynomial.UnivariatePolynomial
import net.ollie.maths.numbers.Precision
import net.ollie.maths.numbers.complex.Complex

import scala.collection.mutable.ArrayBuffer

/**
 * @see [[https://en.wikipedia.org/wiki/Durand-Kerner_method]]
 */
class DurandKerner(val initial: Function[Int, Complex])
    extends UnivariatePolynomialRootFinder {

    def roots(polynomial: UnivariatePolynomial, precision: Precision) = {
        new Solver(polynomial, precision).roots
    }

    private class Solver(polynomial: UnivariatePolynomial, precision: Precision) {

        require(polynomial.degree.toInt.isDefined, () => "Cannot solve for high/infinite degree polynomials: " + polynomial)

        def roots: Seq[Complex] = {
            var prevSolution: Seq[Complex] = Nil
            var solution: Seq[Complex] = Seq.tabulate(polynomial.degree.requireInt)(initial)
            while (!withinPrecision(prevSolution, solution, precision)) {
                prevSolution = solution
                solution = next(solution)
            }
            solution
        }

        def withinPrecision(prev: Seq[Complex], next: Seq[Complex], precision: Precision): Boolean = {
            if (prev == Nil) return false
            require(prev.length == next.length)
            for (i <- 0 to prev.length) {
                val z1 = prev.apply(i)
                val z2 = next.apply(i)
                if (!withinPrecision(z1, z2, precision)) return false
            }
            true
        }

        def withinPrecision(z1: Complex, z2: Complex, precision: Precision): Boolean = {
            precision.within(z1.re.evaluate(precision), z2.re.evaluate(precision)) && precision.within(z1.im.evaluate(precision), z2.im.evaluate(precision))
        }

        def next(current: Seq[Complex]): Seq[Complex] = {
            val next = new ArrayBuffer[Complex](current.length)
            for (i <- current.indices) {
                val z = current.apply(i)
                next += z - (numerator(z) / denominator(i, current))
            }
            next.toSeq
        }

        def numerator(z: Complex): Complex = {
            polynomial.replace(z).as(Complex)
        }

        def denominator(index: Int, coefficients: Seq[Complex]): Complex = {
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

}
