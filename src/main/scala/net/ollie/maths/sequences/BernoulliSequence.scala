package net.ollie.maths.sequences

import net.ollie.maths.numbers.{Precision, Integer, Rational, Natural}
import net.ollie.maths.numbers.constants.{Zero, Half, One}
import org.nevec.rjm.Bernoulli
import net.ollie.maths.methods.ApproximatelyEvaluated

/**
 * Created by Ollie on 19/02/14.
 * @see http://mathworld.wolfram.com/BernoulliNumber.html
 */
object BernoulliSequence
        extends CachingSequence {

    type Element = Rational

    private val calculator = new Bernoulli

    override def apply(n: Natural): Rational = n match {
        case One => -Half
        case _ if !n.isEven => Zero
        case _ => super.apply(n)
    }

    protected[this] def initial = Map(Zero -> One, One -> -Half)

    protected[this] def create(n: Natural): Rational = n.toInt match {
        case Some(m) => new BernoulliNumber(m, calculator)
        case _ => ??? //TODO
    }

    override def toString = "BernoulliSequence"

}

private class BernoulliNumber(val n: Int, val calculator: Bernoulli)
        extends Rational
        with ApproximatelyEvaluated {

    private lazy val rational = calculator.at(n)

    def numerator = Integer(rational.numer())

    def denominator = Integer(rational.denom())

    override protected[this] def doApproximatelyEvaluate(precision: Precision) = {
        numerator.evaluate(precision) / denominator.evaluate(precision)
    }

    override def toString = s"Bernoulli($n)"

}
