package net.ollie.maths.functions.angular

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.Zero
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 09/02/14.
 */
@RunWith(classOf[JUnitRunner])
class HyperbolicSinTest extends FlatSpec with Matchers {

    "Sinh(0)" should "be empty" in {
        HyperbolicSin(0) shouldBe Zero
    }

    "Sinh(1)" should "evaluate" in {
        HyperbolicSin(1).evaluate(4 dp) shouldBe BigDecimal("1.1752")
    }

}
