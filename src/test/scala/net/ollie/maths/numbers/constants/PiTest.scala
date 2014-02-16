package net.ollie.maths.numbers.constants

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.Precision._

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class PiTest extends FlatSpec with Matchers {

    behavior of "Pi"

    it should "evaluate to 8 dp" in {
        Pi.evaluate(8 dp) shouldBe BigDecimal("3.14159265")
    }

    it should "evaluate to 8 sf" in {
        Pi.evaluate(8 sf) shouldBe BigDecimal("3.1415927")
    }

    behavior of "Pi * 2"

    it should "evaluate" in {
        2 * Pi evaluate (4 dp) shouldBe BigDecimal("6.2832")
    }

    behavior of "Pi / 2"

    it should "evaluate to 8 dp" in {
        Pi / 2 evaluate (8 dp) shouldBe BigDecimal("1.57079633")
    }

    it should "evaluate to 8 sf" in {
        Pi / 2 evaluate (8 sf) shouldBe BigDecimal("1.5707963")
    }

}
