package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.Precision._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 24/02/14.
 */
class OmegaTest extends FlatSpec with Matchers {

    behavior of "Omega"

    it should "be strictly positive" in {
        Omega.isPositive shouldBe true
    }

    it should "evaluate to 4 dp" in {
        Omega.evaluate(4 dp) shouldBe BigDecimal("0.5671")
    }

}
