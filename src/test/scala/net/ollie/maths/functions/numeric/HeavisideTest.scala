package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 10/01/14.
 */
@RunWith(classOf[JUnitRunner])
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
