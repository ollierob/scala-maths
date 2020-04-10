package net.ollie.maths.numbers

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 18/01/14.
 */
class PrimeTest extends FlatSpec with Matchers {

    "Pi(100)" should "be 25" in {
        Prime.pi(100) shouldBe (Natural(25))
    }

}
