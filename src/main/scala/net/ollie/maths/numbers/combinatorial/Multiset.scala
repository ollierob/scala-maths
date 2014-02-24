package net.ollie.maths.numbers.combinatorial

import net.ollie.maths.numbers.{Natural, Precision, Real}

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/Multiset.html
 */
object Multiset {

    def choose(n: Natural, k: Natural): Multichoose = new Multichoose(n, k)

    implicit class MultichooseBuilder(val n: Natural) {

        def multichoose(m: Natural) = Multiset.choose(n, m)

    }

    implicit class IntMultichooseBuilder(val i: Int)
            extends MultichooseBuilder(i) {

        def multichoose(j: Int) = Multiset.choose(i, j)

    }

}

class Multichoose(val n: Natural, val k: Natural)
        extends Real {

    private lazy val binomial = BinomialCoefficient(n + k - 1, k)

    def isEmpty = binomial.isEmpty

    def evaluate(precision: Precision) = binomial.evaluate(precision)

    override def toString = s"(($n, $k))"

}
