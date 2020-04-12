package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.numbers.complex.Complex

trait LinearPolynomial
    extends Polynomial {

    override val degree = 1

}

object LinearPolynomial {

    def apply(x: Variable, a: Complex, b: Complex): UnivariatePolynomial = {
        if (a.isZero) Polynomial(x, b)
        else new UnivariateLinearPolynomial(x, a, b)
    }

}

class UnivariateLinearPolynomial(val x: Variable, val a: Complex, val b: Complex)
    extends LinearPolynomial with UnivariatePolynomial {

    require(!a.isZero)

    override def of = x

    override def roots = ???

    override def representation = (a * x) + b

    override def isEmpty = false

    override def df = ???

    override def toString = s"$a.x + $b"

}