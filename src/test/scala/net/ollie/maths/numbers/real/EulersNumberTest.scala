package net.ollie.maths.numbers.real

import net.ollie.maths.numbers.Precision._
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 19/01/14.
 */
@RunWith(classOf[JUnitRunner])
class EulersNumberTest extends FlatSpec with Matchers {

    behavior of "e"

    it should "evaluate to 4 dp" in {
        EulersNumber.evaluate(4 dp) shouldBe BigDecimal("2.7183")
    }

}
