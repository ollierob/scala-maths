package net.ollie.maths.numbers.real

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Ollie on 14/01/14.
 */
class PiTest extends FlatSpec with Matchers {

    behavior of "Pi"

    it should "evaluate" in {
        import net.ollie.maths.numbers.Precision._
        Pi.evaluate(8 dp) shouldBe (BigDecimal("3.14159265"))
    }

}
