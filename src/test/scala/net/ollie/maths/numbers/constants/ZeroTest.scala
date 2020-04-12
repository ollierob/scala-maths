package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.Natural
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 12/01/14.
 */
class ZeroTest extends FlatSpec with Matchers {

    behavior of "Zero"

    it should "be empty" in {
        Zero.isEmpty shouldBe true
    }

    it should "not equal non-empty" in {
        Zero == One shouldBe false
    }

    it should "not be strictly positive" in {
        Zero.isPositive shouldBe false
    }

    it should "be even" in {
        Zero.isEven shouldBe true
    }

    //    it should "invert" in {
    //        val i = Zero.inverse
    //        i.abs shouldBe Infinity
    //        i + 1 shouldBe i
    //    }

    "N(0)" should "equal 0" in {
        val i: Int = 0
        Natural(i) shouldBe Zero
        val i2: BigInt = 0
        Natural(i2) shouldBe Zero
    }

}
