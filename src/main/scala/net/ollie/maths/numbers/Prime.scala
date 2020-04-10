package net.ollie.maths.numbers

import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 18/01/14.
 *
 * @see http://mathworld.wolfram.com/PrimeNumber.html
 */
trait Prime
    extends Natural {

    override def isEmpty = false

    override def isEven = false

}

object Prime {

    private val DEFAULT_CALCULATOR = RjmPrimes

    def apply(n: Natural)(implicit calculator: PrimeCalculator = DEFAULT_CALCULATOR): Option[Prime] = {
        if (is(n)(calculator)) Some(new KnownPrime(n))
        else None
    }

    def is(n: Natural)(implicit calculator: PrimeCalculator = DEFAULT_CALCULATOR): Boolean = n match {
        case Zero => false
        case _: Prime => true
        case _ if n.isEven => false
        case _ => n == next(n)(calculator)
    }

    def next(n: Natural)(implicit calculator: PrimeCalculator = DEFAULT_CALCULATOR): Prime = n match {
        case p: Prime => p
        case _ => new NextPrime(if (n.isEven) n.succ else n)
    }

    /**
     * Prime-counting function.
     */
    def pi(n: Natural)(implicit calculator: PrimeCalculator = DEFAULT_CALCULATOR): Natural = {
        new PrimePi(n)(calculator)
    }

}

private class NextPrime(val n: Natural)(implicit calculator: PrimeCalculator)
    extends AnyRef with Prime {

    private lazy val evaluated = calculator.next(n.evaluate)

    def evaluate = evaluated

    override def toString = s"NextPrime($n)"

}

private class KnownPrime(val n: Natural)
    extends AnyRef with Prime {

    def evaluate = n.evaluate

}

private class PrimePi(val of: Natural)(implicit calculator: PrimeCalculator)
    extends Natural {

    private lazy val evaluated = calculator.pi(of.evaluate)

    def evaluate = evaluated

    override def toString = s"PrimePi($of)"

}

trait PrimeCalculator {

    def next(i: BigInt): BigInt

    def pi(i: BigInt): BigInt

}

object RjmPrimes
    extends PrimeCalculator {

    val prime = new org.nevec.rjm.Prime()

    def next(i: BigInt) = synchronized {
        prime.nextprime(i.underlying())
    }

    def pi(i: BigInt) = synchronized {
        prime.pi(i.underlying())
    }

}