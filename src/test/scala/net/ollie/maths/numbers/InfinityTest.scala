package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 08/01/14.
 */
@RunWith(classOf[JUnitRunner])
class InfinityTest extends FlatSpec with Matchers {

    behavior of "Infinity"

    it should "have a multiplicative inverse of 0" in {
        Infinity.inverse shouldBe (Zero)
    }

    it should "add to a non-infinite number" in {
        Infinity + 5 shouldBe (Infinity)
    }

    it should "add to +infinity" in {
        Infinity + Infinity shouldBe (Infinity)
    }

    it should "multiply by +infinity" in {
        Infinity * Infinity shouldBe (Infinity)
    }

    it should "multiply by -infinity" in {
        Infinity * -Infinity shouldBe (-Infinity)
    }

}
