package net.ollie.maths.numbers

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.Pi
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Created by Ollie on 15/01/14.
 */
class RealProductTest extends AnyFlatSpec with Matchers {

    "small * large" should "evaluate" in {
        val large: Real = Real(999961)
        val small: Real = 1 / Real(999983)
        val product = RealProduct(Seq(small, large))
        product.evaluate(4 dp) shouldBe BigDecimal("1.0000")
        product.evaluate(5 dp) shouldBe BigDecimal("0.99998")
    }

    "2 * Pi" should "evaluate" in {
        RealProduct(Seq(2, Pi)).evaluate(16 dp) shouldBe BigDecimal("6.2831853071795865")
    }

}
