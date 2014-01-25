package net.ollie.maths.numbers.real.combinatorial

import net.ollie.maths.numbers.{Natural, Precision, Real}

/**
 * Created by Ollie on 22/01/14.
 */
object Multiset {

    def apply(n: Natural, k: Natural): Real = new Multiset(n, k)

}

class Multiset(val n: Natural, k: Natural)
        extends Real {

    private val binomial = BinomialCoefficient(n + k - 1, k)

    protected[this] def eval(precision: Precision) = binomial.evaluate(precision)

    def isEmpty = binomial.isEmpty

    override def toString = s"Multiset($n, $k)"

}