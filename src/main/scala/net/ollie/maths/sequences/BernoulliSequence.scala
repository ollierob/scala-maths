package net.ollie.maths.sequences

import net.ollie.maths.numbers.{Integer, Rational, Natural}
import net.ollie.maths.numbers.constants.{Zero, Half, One}
import org.nevec.rjm.Bernoulli

/**
 * Created by Ollie on 19/02/14.
 * @see http://mathworld.wolfram.com/BernoulliNumber.html
 */
object BernoulliSequence
        extends CachingSequence {

    type Element = Rational

    private val bernoulli = new Bernoulli

    override def apply(n: Natural): Rational = n match {
        case One => -Half
        case _ if !n.isEven => Zero
        case _ => super.apply(n)
    }

    protected[this] def initial = Map(Zero -> One, One -> -Half)

    protected[this] def create(n: Natural): Rational = n.toInt match {
        case Some(m) => new BernoulliNumber(m, bernoulli)
        case _ => ??? //TODO
    }

    override def toString = "BernoulliSequence"

}

private class BernoulliNumber(val n: Int, val b: Bernoulli)
        extends Rational {

    private lazy val rational = b.at(n)

    def numerator = Integer(rational.numer())

    def denominator = Integer(rational.denom())

    override def toString = s"Bernoulli($n)"

}
