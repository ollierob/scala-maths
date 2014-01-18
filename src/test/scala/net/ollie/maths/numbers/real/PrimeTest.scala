package net.ollie.maths.numbers.real

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.NaturalNumber

/**
 * Created by Ollie on 18/01/14.
 */
class PrimeTest extends FlatSpec with Matchers {

    "Pi(100)" should "be 25" in {
        Prime.pi(100) shouldBe (NaturalNumber(25))
    }

}
