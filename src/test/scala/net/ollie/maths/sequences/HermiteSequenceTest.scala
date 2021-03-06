package net.ollie.maths.sequences

import net.ollie.maths.numbers.Integer
import net.ollie.maths.numbers.constants.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 25/02/14.
 */
class HermiteSequenceTest extends FlatSpec with Matchers {

    "H(odd)" should "be zero" in {
        HermiteSequence(1) shouldBe Zero
        HermiteSequence(3) shouldBe Zero
        HermiteSequence(5) shouldBe Zero
        HermiteSequence(7) shouldBe Zero
    }

    "H(0)" should "be 1" in {
        HermiteSequence(0) shouldBe One
    }

    "H(2)" should "be -2" in {
        HermiteSequence(2) shouldBe Integer(-2)
    }

    "H(4)" should "be 12" in {
        HermiteSequence(4) shouldBe Integer(12)
    }

    "H(80)" should "be positive and even" in {
        HermiteSequence(80).isPositive shouldBe true
        HermiteSequence(80).isEven shouldBe true
    }

}
