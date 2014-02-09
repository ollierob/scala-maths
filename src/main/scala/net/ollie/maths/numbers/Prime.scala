package net.ollie.maths.numbers

import net.ollie.maths.numbers.Natural

/**
 * Created by Ollie on 18/01/14.
 */
trait Prime
        extends Natural

object Prime {

    private val DEFAULT_CALCULATOR = RjmPrimes

    def apply(n: Natural): Option[Prime] = if (is(n)) Some(new KnownPrime(n)) else None

    def is(n: Natural)(implicit calculator: PrimeCalculator = DEFAULT_CALCULATOR): Boolean = n match {
        case p: Prime => true
        case _ if n.isEven => false
        case _ => n == new NextPrime(n)
    }

    def next(n: Natural)(implicit calculator: PrimeCalculator = DEFAULT_CALCULATOR): Prime = n match {
        case p: Prime => p
        case _ => new NextPrime(if (n.isEven) n.succ else n)
    }

    def pi(n: Natural)(implicit calculator: PrimeCalculator = DEFAULT_CALCULATOR): Natural = {
        Natural(calculator.pi(n.evaluate))
    }

}

private class NextPrime(val n: Natural)(implicit calculator: PrimeCalculator)
        extends AnyRef
        with Prime {

    private lazy val evaluated = calculator.next(n.evaluate)

    def evaluate = evaluated

    override def toString = s"NextPrime($n)"

}

private class KnownPrime(val n: Natural)
        extends AnyRef
        with Prime {

    def evaluate = n.evaluate

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

//private object ConcurrentPrimes
//        extends PrimeCalculator {
//
//    private val primes: collection.mutable.Map[BigInt, Prime] = (new ConcurrentHashMap[BigInt, Prime] asScala)
//    primes.put(1, new KnownPrime(2))
//    primes.put(2, new KnownPrime(3))
//    primes.put(3, new KnownPrime(5))
//    primes.put(4, new KnownPrime(7))
//    primes.put(5, new KnownPrime(11))
//    primes.put(6, new KnownPrime(13))
//
//    def next(i: BigInt): BigInt = ???
//
//    def pi(i: BigInt): BigInt = ???
//
//    /**
//     * Expand the cache so that the largest prime is greater than or equal to the given int.
//     * @param i
//     */
//    def growTo(i: BigInt) {
//        if (max.evaluate < i) {
//
//        }
//    }
//
//    def max: Prime = {
//        primes.get(primes.keys.max).get
//    }
//
//}