package net.ollie.maths.sequences

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.numbers.{IntegerFraction, Natural}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Created by Ollie on 19/02/14.
 */
class BernoulliMinusSequenceTest extends AnyFlatSpec with Matchers {

    "Bernoulli-(3)" should "be 0" in {
        BernoulliMinusSequence(3) shouldBe Zero
    }

    "Bernoulli-(4)" should "be -1/30" in {
        val b = BernoulliMinusSequence(4)
        b shouldBe IntegerFraction(-1, 30)
    }

    "Bernoulli-(5)" should "be 0" in {
        BernoulliMinusSequence(5) shouldBe Zero
    }

    "Bernoulli-(6)" should "be 1/42" in {
        val b = BernoulliMinusSequence(6)
        b.numerator shouldBe One
        b.denominator shouldBe Natural(42)
        b shouldBe IntegerFraction(1, 42)
        b.evaluate(4 dp) shouldBe BigDecimal("0.0238")
    }

}
