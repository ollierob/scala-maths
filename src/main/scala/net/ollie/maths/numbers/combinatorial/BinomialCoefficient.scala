package net.ollie.maths.numbers.combinatorial

import net.ollie.maths.{Empty, Variable}
import net.ollie.maths.functions.numeric.{Floor, GreatestCommonDivisor}
import net.ollie.maths.numbers._
import net.ollie.maths.methods.Product

/**
 * Created by Ollie on 11/01/14.
 */
trait BinomialCoefficient
        extends NaturalNumber {

    def n: NaturalNumber

    def possibilities = n

    def k: NaturalNumber

    def outcomes = k

}

object BinomialCoefficient {

    def apply(n: NaturalNumber, k: NaturalNumber): BinomialCoefficient = {
        if (n >= k) new BinomialAny(n, k)
        else new BinomialZero(n, k)
    }

    def central(n: NaturalNumber) = apply(n, Floor(n / 2))

    /**
     * Coefficient is good if LeastPrimeFactor(n choose k) > k
     * @see http://mathworld.wolfram.com/GoodBinomialCoefficient.html
     **/
    def isGood(c: BinomialCoefficient): Boolean = c.k >= 2 && GreatestCommonDivisor(c, c.k !) == One

    implicit class BinomialCoefficientIntBuilder(n: Int) extends AnyRef {

        def choose(k: Int): BinomialCoefficient = BinomialCoefficient(n, k)

    }

    implicit class BinomialCoefficientBuilder(n: NaturalNumber) extends AnyRef {

        def choose(k: NaturalNumber): BinomialCoefficient = BinomialCoefficient(n, k)

    }

}

class BinomialZero(val n: NaturalNumber, val k: NaturalNumber)
        extends BinomialCoefficient
        with Empty {

    require(n < k)

    def evaluate = 0

    override def unary_-() = this

    override def isEmpty = true

    override def variables = super[Empty].variables

    override def df(x: Variable) = Zero

}

class BinomialAny(val n: NaturalNumber, val k: NaturalNumber)
        extends BinomialCoefficient {

    require(n >= k)

    def nMinusK: NaturalNumber = NaturalNumber(n - k).right.get

    private def multiplicative(i: IntegerNumber): RealNumber = (n - k + i) / i

    private lazy val evaluated: BigInt = Product(multiplicative, 1, k).evaluate(IntegerPrecision).toBigIntExact.get

    def evaluate = evaluated

}
