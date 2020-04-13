package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.BivariateFunction
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.combinatorial.BinomialCoefficient._
import net.ollie.maths.numbers.constants.{Half, One, Zero}
import net.ollie.maths.numbers.{Natural, Rational, Real}
import net.ollie.maths.sequences.BernoulliPlusSequence

/**
 * Created by Ollie on 08/03/14.
 *
 * @see http://mathworld.wolfram.com/BernoulliPolynomial.html
 * @see [[net.ollie.maths.sequences.BernoulliSequence]]
 */
trait BernoulliPolynomial
    extends Polynomial {

    override type Coefficient = Rational

    override def coefficient(k: Natural) = (degree choose k) * BernoulliPlusSequence(Natural.require(degree - k))

    override def toString = s"BernoulliPolynomial($degree)($of)" //B_n(x)

}

object BernoulliPolynomial
    extends BivariateFunction[Natural, Real, Real] {

    def apply(degree: Natural)(x: Expression): BernoulliPolynomial = degree match {
        case Zero => new KnownBernoulliPolynomial(0, x, One)
        case One => new KnownBernoulliPolynomial(1, x, x - Half)
        case _ => new ComputedBernoulliPolynomial(degree, x)
    }

    override def apply(n: Natural, re: Real): Real = Real(apply(n)(re).toConstant) match {
        case Some(re) => re
        case _ => ???
    }

}

private class KnownBernoulliPolynomial(val degree: Natural, val of: Expression, val representation: Expression)
    extends BernoulliPolynomial

private class ComputedBernoulliPolynomial(val degree: Natural, val of: Expression)
    extends BernoulliPolynomial {


    def representation = Series(nth _, Zero, degree)

    private def nth(k: Natural): Expression = coefficient(k) * (of ^ k)

}