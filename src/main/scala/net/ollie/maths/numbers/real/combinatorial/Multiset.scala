package net.ollie.maths.numbers.real.combinatorial

import net.ollie.maths.numbers.{NaturalNumber, Precision, RealNumber}

/**
 * Created by Ollie on 22/01/14.
 */
object Multiset {

    def apply(n: NaturalNumber, k: NaturalNumber): RealNumber = new Multiset(n, k)

}

class Multiset(val n: NaturalNumber, k: NaturalNumber)
        extends RealNumber {

    private val binomial = BinomialCoefficient(n + k - 1, k)

    protected[this] def eval(precision: Precision) = binomial.evaluate(precision)

    def isEmpty = binomial.isEmpty

    override def toString = s"Multiset($n, $k)"

}