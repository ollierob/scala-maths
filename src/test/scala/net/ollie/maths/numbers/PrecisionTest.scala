package net.ollie.maths.numbers

import net.ollie.maths.numbers.Precision._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Created by Ollie on 25/01/14.
 */
class PrecisionTest extends AnyFlatSpec with Matchers {

    behavior of "4 significant figures"

    {

        val precision = 4 sf

        it should "evaluate 1 to 4 sig figs" in {
            val input = BigDecimal("1")
            val output = input to precision
            output shouldBe BigDecimal("1.000")
            output.underlying() shouldBe new java.math.BigDecimal("1.000")
        }

        it should "evaluate 1.2345 to 4 sig figs" in {
            val input = BigDecimal("1.2341")
            val output = input to precision
            output shouldBe BigDecimal("1.234")
            output.underlying() shouldBe new java.math.BigDecimal("1.234")
        }

        it should "be geq than itself" in {
            precision == precision shouldBe true
            precision >= precision shouldBe Some(true)
        }

        it should "tolerate difference" in {

            val d1 = BigDecimal(123456)
            precision.within(d1, d1) shouldBe true

            val d2 = BigDecimal(123457)
            precision.within(d1, d2) shouldBe true
            precision.within(d1, d1) shouldBe true

            val d3 = BigDecimal(5)
            precision.within(d1, d3) shouldBe false
            precision.within(d3, d1) shouldBe false

        }

    }

    behavior of "4 decimal places"

    {

        val precision = 4 dp

        it should "tolerate difference" in {

            val d1 = BigDecimal(1)
            precision.within(d1, d1) shouldBe true

            val d2 = BigDecimal(2)
            precision.within(d1, d2) shouldBe false
            precision.within(d2, d1) shouldBe false

            val d3 = BigDecimal(1.00001)
            precision.within(d1, d3) shouldBe true

        }

    }

}
