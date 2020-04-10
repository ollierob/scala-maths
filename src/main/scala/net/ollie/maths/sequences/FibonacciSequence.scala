package net.ollie.maths.sequences

import net.ollie.maths.numbers.{IntegerPrecision, Integer, Natural}
import net.ollie.maths.numbers.constants.{GoldenRatio, MinusOne, Zero, One}
import net.ollie.maths.functions.numeric.PositiveSquareRoot

/**
 * Created by Ollie on 19/02/14.
 */
object FibonacciSequence
        extends CachingSequence {

    type Element = Natural

    override def apply(n: Natural): Natural = super.apply(n)

    def apply(i: Integer): Natural = i match {
        case n: Natural => apply(n)
        case _ => {
            val n: Natural = i.abs
            (MinusOne ^ (n.succ)) * FibonacciSequence(n)
        }
    }

    def apply(i: Int): Natural = apply(Integer(i))

    protected[this] def initial = Map(Zero -> Zero, One -> One)

    private val large: Natural = 1024

    protected[this] def create(n: Natural) = {
        if (n > large) new LargeFibonacciNumber(n)
        else new FibonacciNumber(n)
    }

}

class FibonacciNumber(val n: Natural)
        extends Natural {

    def evaluate = FibonacciSequence(n - 1).evaluate + FibonacciSequence(n - 2).evaluate

    override def toString = s"Fibonacci($n)"

}

class LargeFibonacciNumber(val n: Natural)
        extends Natural {

    private lazy val f = ((GoldenRatio ^ n) - ((-GoldenRatio) ^ n)) / PositiveSquareRoot(5)

    def evaluate: BigInt = f.evaluate(IntegerPrecision).toBigInt

    override def toString = s"Fibonacci($n)"

}