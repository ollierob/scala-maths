package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.Precision._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 19/01/14.
 */
class EulersNumberTest extends FlatSpec with Matchers {

    behavior of "e"

    it should "evaluate to 4 dp" in {
        EulersNumber.evaluate(4 dp) shouldBe BigDecimal("2.7183")
    }

}
