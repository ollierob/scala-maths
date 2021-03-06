package net.ollie.maths.numbers.combinatorial

import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 11/01/14.
 */
class BinomialCoefficientTest extends FlatSpec with Matchers {

    import BinomialCoefficient._

    "5 choose 2" should "equal 10" in {
        BinomialCoefficient(5, 2) shouldBe (Natural(10))
        5 choose 2 shouldBe (Natural(10))
    }

    "5 choose 6" should "equal 0" in {
        5 choose 6 shouldBe (Zero)
    }

    "0 choose 0" should "equal 1" in {
        BinomialCoefficient(0, 0) shouldBe (One)
        0 choose 0 shouldBe (One)
    }

    "3 choose 2" should "be good" in {
        BinomialCoefficient.isGood(3 choose 2) shouldBe (true)
    }

    "5 choose 3" should "not be good" in {
        BinomialCoefficient.isGood(5 choose 3) shouldBe (false)
    }

    "5 choose 4" should "be good" in {
        BinomialCoefficient.isGood(5 choose 4) shouldBe (true)
    }

}
