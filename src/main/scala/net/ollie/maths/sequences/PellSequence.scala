package net.ollie.maths.sequences

import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One}
import net.ollie.maths.functions.numeric.PositiveSquareRoot

/**
 * Created by Ollie on 24/02/14.
 * @see http://mathworld.wolfram.com/PellNumber.html
 * @see http://oeis.org/A000129
 */
object PellSequence
        extends CachingSequence {

    private val large: Natural = 1024

    type Element = Natural

    protected[this] def initial = Map(Zero -> Zero, One -> One)

    protected[this] def create(n: Natural): PellNumber = new SmallPellNumber(n)

    override protected[this] def shouldCache(n: Natural) = n < large

    override protected[this] def createNoCache(n: Natural): PellNumber = new LargePellNumber(n)

}

trait PellNumber
        extends Natural {

    val n: Natural

    override def toString = s"Pell($n)"

}

private class SmallPellNumber(val n: Natural)
        extends PellNumber {

    private lazy val precursor: Integer = (2 * PellSequence(n - 1)) + PellSequence(n - 2)

    override def evaluate = precursor.evaluate

}

private class LargePellNumber(val n: Natural)
        extends PellNumber {

    private lazy val binet = (((1 + PositiveSquareRoot(2)) ^ n) + ((1 - PositiveSquareRoot(2)) ^ n)) / (2 * PositiveSquareRoot(2))

    override def evaluate = binet.evaluate(IntegerPrecision).toBigInt

}
