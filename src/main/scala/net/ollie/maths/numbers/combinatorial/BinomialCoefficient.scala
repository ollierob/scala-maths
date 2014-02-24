package net.ollie.maths.numbers.combinatorial

import net.ollie.maths.{EmptyNumber, Variable}
import net.ollie.maths.functions.numeric.{Floor, GreatestCommonDivisor}
import net.ollie.maths.numbers._
import net.ollie.maths.methods.Product
import net.ollie.maths.numbers.constants.{MinusOne, Zero, One}

/**
 * Created by Ollie on 11/01/14.
 */
trait BinomialCoefficient
        extends Natural {

    def n: Natural

    def possibilities = n

    def k: Natural

    def outcomes = k

}

object BinomialCoefficient {

    import Multiset._

    def apply(i: Int, j: Int): BinomialCoefficient = apply(Natural(i), Natural(j))

    def apply(n: Integer, k: Integer): Real = (n, k) match {
        case _ if n.isStrictlyPositive && k.isStrictlyPositive => apply(n.abs, k.abs)
        case _ => (MinusOne ^ k) * (n.abs multichoose k)
    }

    def apply(n: Natural, k: Natural): BinomialCoefficient = {
        if (n >= k) new BinomialAny(n, k)
        else new BinomialZero(n, k)
    }

    def central(n: Integer): Real = apply(n, Floor(n / 2))

    /**
     * Coefficient is good if LeastPrimeFactor(degree choose k) > k
     * @see http://mathworld.wolfram.com/GoodBinomialCoefficient.html
     **/
    def isGood(c: BinomialCoefficient): Boolean = c.k >= 2 && GreatestCommonDivisor(c, c.k !) == One

    implicit class BinomialCoefficientIntBuilder(n: Int) extends AnyRef {

        def choose(k: Int): BinomialCoefficient = BinomialCoefficient(n, k)

    }

    implicit class BinomialCoefficientBuilder(n: Natural) extends AnyRef {

        def choose(k: Natural): BinomialCoefficient = BinomialCoefficient(n, k)

    }

}

class BinomialZero(val n: Natural, val k: Natural)
        extends BinomialCoefficient
        with EmptyNumber {

    require(n < k)

    def evaluate = 0

    override def abs = Zero

    override def unary_-() = this

    override def isEmpty = true

    override def variables = super[EmptyNumber].variables

    override def evaluate(precision: Precision) = super[EmptyNumber].evaluate(precision)

    override def df(x: Variable) = Zero

}

class BinomialAny(val n: Natural, val k: Natural)
        extends BinomialCoefficient {

    require(n >= k)

    def nMinusK: Natural = Natural(n - k).right.get

    private def multiplicative(i: Integer): Real = (n - k + i) / i

    private lazy val evaluated: BigInt = Product(multiplicative, 1, k).evaluate(IntegerPrecision).toBigIntExact.get

    def evaluate = evaluated

}
