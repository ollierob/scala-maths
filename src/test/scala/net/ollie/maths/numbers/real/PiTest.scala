package net.ollie.maths.numbers.real

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class PiTest extends FlatSpec with Matchers {

    behavior of "Pi"

    it should "evaluate" in {
        import net.ollie.maths.numbers.Precision._
        Pi.evaluate(8 dp) shouldBe (BigDecimal("3.14159265"))
    }

}
