package net.ollie.maths.sequences

import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.combinatorial.BinomialCoefficient._
import net.ollie.maths.numbers.constants._
import net.ollie.maths.numbers.{IntegerFraction, Natural, Precision, Rational}
import net.ollie.maths.tensors.KroneckerDelta

/**
 * Created by Ollie on 19/02/14.
 *
 * @see http://mathworld.wolfram.com/BernoulliNumber.html
 */
sealed trait BernoulliSequence
    extends CachingSequence {

    type Element = Rational

    def oneConvention: Rational

    override def apply(n: Natural): Rational = n match {
        case Zero => One
        case One => oneConvention
        case _ if n.isOdd => Zero
        case _ => super.apply(n)
    }

    override protected[this] def shouldCache(n: Natural) = n.isEven

    protected[this] def initial = Map(Zero -> One, One -> oneConvention, Two -> IntegerFraction(1, 6), Three -> Zero)

}

/**
 * Modern convention with B(1) = -1/2
 */
object BernoulliMinusSequence
    extends BernoulliSequence {

    override def oneConvention = -Half

    override protected[this] def create(n: Natural): Rational = new RecursiveBernoulliMinusNumber(n)

    override def toString = "BernoulliMinusSequence"

}

private class RecursiveBernoulliMinusNumber(val n: Natural)
    extends Rational {

    private lazy val recursion = KroneckerDelta(n, 0) - Series((k: Natural) => (n choose k) * BernoulliMinusSequence(k) / (n - k + 1), Zero, n.decr)
    private lazy val rationalVal = Rational(recursion).get //FIXME tighter typing above

    override lazy val numerator = rationalVal.numerator
    override lazy val denominator = rationalVal.denominator

    override def evaluate(precision: Precision): BigDecimal = recursion.evaluate(precision)

    override def toString = s"Bernoulli-($n)"

}

/**
 * Old convention with B(1) = 1/2
 */
object BernoulliPlusSequence
    extends BernoulliSequence {

    override def oneConvention = Half

    override def toString = "BernoulliPlusSequence"

    override protected[this] def create(n: Natural): Rational = new RecursiveBernoulliPlusNumber(n)

}

private class RecursiveBernoulliPlusNumber(val n: Natural)
    extends Rational {

    private lazy val recursion = 1 - Series((k: Natural) => (n choose k) * BernoulliPlusSequence(k) / (n - k + 1), Zero, n.decr)
    private lazy val rationalVal = Rational(recursion).get //FIXME tighter typing above

    override lazy val numerator = rationalVal.numerator
    override lazy val denominator = rationalVal.denominator

    override def evaluate(precision: Precision): BigDecimal = recursion.evaluate(precision)

    override def toString = s"Bernoulli+($n)"

}