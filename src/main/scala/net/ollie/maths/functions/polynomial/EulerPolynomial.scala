package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.Expression
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.functions.{BivariateFunction, Represented}

/**
 * Created by Ollie on 04/03/14.
 * @see http://mathworld.wolfram.com/EulerPolynomial.html
 */
trait EulerPolynomial
        extends Polynomial {

    override def toString = s"EulerPolynomial($degree)($of)"

}

object EulerPolynomial
        extends BivariateFunction[Natural, Real, Real] {

    def apply(order: Natural)(x: Expression): EulerPolynomial = {
        new EulerPolynomialOf(order)(x)
    }

    def apply(order: Natural, re: Real): Real = Real(apply(order)(re).toConstant) match {
        case Some(re) => re
        case _ => ???
    }

}

class EulerPolynomialOf(val degree: Natural)(val of: Expression)
        extends EulerPolynomial
        with Represented {

    private val n = degree.succ

    def representation = {
        2 * (BernoulliPolynomial(n)(of) - ((2 ^ n) * BernoulliPolynomial(n)(of / 2))) / n
    }

}