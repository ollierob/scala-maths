package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Integer
import net.ollie.maths.numbers.Precision._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 16/02/14.
 */
@RunWith(classOf[JUnitRunner])
class ModuloTest extends FlatSpec with Matchers {

    "10 % 3" should "be 1" in {
        val m = Modulo(10, 3)
        m.evaluate(4 dp) shouldBe BigDecimal("1.0000")
        m shouldBe Integer(1)
    }

    "10 % 11" should "be 0" in {

    }

}
