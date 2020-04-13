package net.ollie.maths.sequences

import net.ollie.maths.numbers.{IntegerFraction, Natural}
import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.{MinusOne, One, Zero}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Created by Ollie on 19/02/14.
 */
class BernoulliPlusSequenceTest extends AnyFlatSpec with Matchers {

    "Bernoulli+(2)" should "be 1/6" in {
        val b = BernoulliPlusSequence(2)
        b shouldBe IntegerFraction(1, 6)
    }

    "Bernoulli+(3)" should "be 0" in {
        BernoulliPlusSequence(3) shouldBe Zero
    }

    "Bernoulli+(4)" should "be -1/30" in {
        val b = BernoulliPlusSequence(4)
        b.denominator shouldBe Natural(30)
        b.numerator shouldBe MinusOne
        b shouldBe IntegerFraction(-1, 30)
    }

    "Bernoulli+(5)" should "be 0" in {
        BernoulliPlusSequence(5) shouldBe Zero
    }

    "Bernoulli+(6)" should "be 1/42" in {
        val b = BernoulliPlusSequence(6)
        b.numerator shouldBe One
        b.denominator shouldBe Natural(42)
        b shouldBe IntegerFraction(1, 42)
        b.evaluate(4 dp) shouldBe BigDecimal("0.0238")
    }

}
