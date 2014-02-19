package net.ollie.maths.sequences

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.{Integer, IntegerFraction}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 19/02/14.
 */
@RunWith(classOf[JUnitRunner])
class BernoulliSequenceTest extends FlatSpec with Matchers {

    "Bernoulli(6)" should "be 1/42" in {
        val b = BernoulliSequence(6)
        b.numerator shouldBe Integer(1)
        b.denominator shouldBe Integer(42)
        b.evaluate(4 dp) shouldBe BigDecimal("0.0238")
        b shouldBe IntegerFraction(1, 42)
    }

}
