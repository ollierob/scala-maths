package net.ollie.maths.numbers.combinatorial

import net.ollie.maths.numbers.{Zero, NaturalNumber, One}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 11/01/14.
 */
class BinomialCoefficientTest extends FlatSpec with Matchers {

    import BinomialCoefficient._

    "5 choose 2" should "equal 10" in {
        BinomialCoefficient(5, 2) shouldBe (NaturalNumber(10))
        5 choose 2 shouldBe (NaturalNumber(10))
    }

    "5 choose 6" should "equal 0" in {
        5 choose 6 shouldBe (Zero)
    }

    "0 choose 0" should "equal 1" in {
        BinomialCoefficient(0, 0) shouldBe (One)
        0 choose 0 shouldBe (One)
    }

    "3 choose 2" should "be good" in {
        (3 choose 2).isGood shouldBe (true)
    }

    "5 choose 3" should "not be good" in {
        (5 choose 3).isGood shouldBe (false)
    }

    "5 choose 4" should "be good" in {
        (5 choose 4).isGood shouldBe (true)
    }

}
