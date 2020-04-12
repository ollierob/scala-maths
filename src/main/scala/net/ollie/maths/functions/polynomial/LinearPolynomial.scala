package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.expressions.Expression
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{One, Zero}

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

    override def derivative = Polynomial(x, a)

    override def equals(expr: Expression) = expr match {
        case p: UnivariateLinearPolynomial => x == p.x && a == p.a && b == p.b
        case _ => super.equals(expr)
    }

    override def coefficient(power: Natural) = power match {
        case Zero => b
        case One => a
        case _ => 0
    }

    override def toString = s"($a.x + $b)"

}