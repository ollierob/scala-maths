package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.constants.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 10/01/14.
 */
class HeavisideTest extends FlatSpec with Matchers {

    "H(-1)" should "= 0" in {
        Heaviside(-1) shouldBe (Zero)
    }

    "H(0)" should "= 1/2" in {
        Heaviside(0) shouldBe (One / 2)
    }

    "H(1)" should "=1" in {
        Heaviside(1) shouldBe (One)
    }

}
