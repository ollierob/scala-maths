package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.Expression
import net.ollie.maths.numbers.{Integer, Natural}
import net.ollie.maths.numbers.constants.{MinusOne, One, Zero}
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.methods.Differentiate
import net.ollie.maths.sequences.HermiteSequence

/**
 * Created by Ollie on 25/02/14.
 */
trait HermitePolynomial
        extends Polynomial {

    override def toString = s"HermiteP($degree)($of)"

}

object HermitePolynomial {

    def apply(n: Natural): Integer = HermiteSequence(n)

    def apply(n: Natural)(of: Expression): HermitePolynomial = {
        n match {
            case Zero => new ZeroHermitePolynomial(of)
            case _ => new HermitePolynomialOf(n)(of)
        }
    }

}

class ZeroHermitePolynomial(val of: Expression)
        extends ConstantPolynomial(One)
        with HermitePolynomial {

    def degree = Zero

}

class HermitePolynomialOf(val degree: Natural)(val of: Expression)
        extends HermitePolynomial {

    private lazy val repr: Expression = {
        if (of.isEmpty) HermiteSequence(degree)
        else (MinusOne ^ degree) * Exp(of ^ 2) * Differentiate(Exp((-of) ^ 2), degree)
    }

    def representation = repr

}