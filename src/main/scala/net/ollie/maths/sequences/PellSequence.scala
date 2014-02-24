package net.ollie.maths.sequences

import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One}
import net.ollie.maths.functions.numeric.PositiveSquareRoot

/**
 * Created by Ollie on 24/02/14.
 * @see http://mathworld.wolfram.com/PellNumber.html
 */
object PellSequence
        extends CachingSequence {

    type Element = Natural

    protected[this] def initial = Map(Zero -> Zero, One -> One)

    protected[this] def create(n: Natural) = new SmallPellNumber(n)

}

trait PellNumber
        extends Natural {

    val n: Natural

    override def toString = s"Pell($n)"

}

class SmallPellNumber(val n: Natural)
        extends PellNumber {

    private lazy val precursor: Integer = (2 * PellSequence(n - 1)) + PellSequence(n - 2)

    override def evaluate = precursor.evaluate

}

class LargePellNumber(val n: Natural)
        extends PellNumber {

    private lazy val binet = (((1 + PositiveSquareRoot(2)) ^ n) + ((1 - PositiveSquareRoot(2)) ^ n)) / (2 * PositiveSquareRoot(2))

    protected[this] def evaluate = binet.evaluate(IntegerPrecision).toBigInt

}
