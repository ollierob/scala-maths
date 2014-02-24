package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import Precision._

/**
 * Created by Ollie on 25/01/14.
 */
class PrecisionTest extends FlatSpec with Matchers {

    behavior of "Significant figures"

    it should "evaluate 1 to 4 sig figs" in {
        val precision = 4 sf
        val input = BigDecimal("1")
        val output = input to precision
        output shouldBe BigDecimal("1.000")
        output.underlying() shouldBe new java.math.BigDecimal("1.000")
    }

    it should "evaluate 1.2345 to 4 sig figs" in {
        val precision = 4 sf
        val input = BigDecimal("1.2341")
        val output = input to precision
        output shouldBe BigDecimal("1.234")
        output.underlying() shouldBe new java.math.BigDecimal("1.234")
    }

    it should "be geq than itself" in {
        val precision = SignificantFigures(4)
        precision == precision shouldBe true
        precision >= precision shouldBe Some(true)
    }

}
