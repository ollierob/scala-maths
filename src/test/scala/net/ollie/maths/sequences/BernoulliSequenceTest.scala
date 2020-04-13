package net.ollie.maths.sequences

import net.ollie.maths.numbers.IntegerFraction
import net.ollie.maths.numbers.Precision._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Created by Ollie on 19/02/14.
 */
class BernoulliSequenceTest extends AnyFlatSpec with Matchers {

    "Bernoulli(2)" should "be 1/6" in {
        val b = BernoulliPlusSequence(2)
        b shouldBe IntegerFraction(1, 6)
    }

    "Bernoulli(6)" should "be 1/42" in {
        val b = BernoulliPlusSequence(6)
        b shouldBe IntegerFraction(1, 42)
        b.evaluate(4 dp) shouldBe BigDecimal("0.0238")
    }

}
