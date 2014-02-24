package net.ollie.maths.numbers.constants

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Precision._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * Created by Ollie on 24/02/14.
 */
@RunWith(classOf[JUnitRunner])
class OmegaTest extends FlatSpec with Matchers {

    behavior of "Omega"

    it should "be strictly positive" in {
        Omega.isStrictlyPositive shouldBe true
    }

    it should "evaluate to 4 dp" in {
        Omega.evaluate(4 dp) shouldBe BigDecimal("0.5671")
    }

}
