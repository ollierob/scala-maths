package net.ollie.maths.sequences

import net.ollie.maths.numbers.{Real, IntegerPrecision, Integer, Natural}
import net.ollie.maths.numbers.constants._
import net.ollie.maths.functions.numeric.{Modulo, Floor}

/**
 * Created by Ollie on 25/02/14.
 */
object HermiteSequence
        extends CachingSequence {

    type Element = Integer

    protected[this] def initial = Map(Zero -> One, Natural(2) -> -2)

    protected[this] def create(n: Natural) = new EvenHermiteNumber(n)

    override protected[this] def shouldCache(n: Natural) = n.isEven

    override protected[this] def createNoCache(n: Natural) = Zero

}

trait HermiteNumber
        extends Integer {

    def n: Natural

    override def toString = s"Hermite($n)"

}

class EvenHermiteNumber(val n: Natural)
        extends HermiteNumber {

    require(n.isEven)

    private lazy val halfN: Natural = Floor(n / 2)

    private lazy val evaluated: Real = (MinusOne ^ halfN) * (n !) / (halfN !)

    def evaluate = evaluated.evaluate(IntegerPrecision).toBigInt

    override def isEven = !n.isEmpty

    override def isPositive = Modulo(n, 4).remainder.isEmpty

}
