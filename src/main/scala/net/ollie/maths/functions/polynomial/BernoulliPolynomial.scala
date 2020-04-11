package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.Expression
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.methods.Series
import net.ollie.maths.sequences.BernoulliSequence
import net.ollie.maths.functions.BivariateFunction

/**
 * Created by Ollie on 08/03/14.
 * @see http://mathworld.wolfram.com/BernoulliPolynomial.html
 * @see [[net.ollie.maths.sequences.BernoulliSequence]]
 */
trait BernoulliPolynomial
        extends Polynomial {

    override def toString = s"BernoulliPolynomial($degree)($of)"

}

object BernoulliPolynomial
        extends BivariateFunction[Natural, Real, Real] {

    def apply(n: Natural)(x: Expression): BernoulliPolynomial = {
        new BernoulliPolynomialOf(n)(x)
    }

    def apply(n: Natural, re: Real): Real = Real(apply(n)(re).toConstant) match {
        case Some(re) => re
        case _ => ???
    }

}

class BernoulliPolynomialOf(val degree: Natural)(val of: Expression)
        extends BernoulliPolynomial {

    import net.ollie.maths.numbers.combinatorial.BinomialCoefficient._

    def representation = Series(nth _, Zero, degree)

    private def nth(k: Natural): Expression = {
        (degree choose k) * BernoulliSequence(degree - k) * (of ^ k)
    }

}