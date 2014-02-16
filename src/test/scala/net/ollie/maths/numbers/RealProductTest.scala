package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Precision._

/**
 * Created by Ollie on 15/01/14.
 */
@RunWith(classOf[JUnitRunner])
class RealProductTest extends FlatSpec with Matchers {

    "small * large" should "evaluate" in {
        val large: Real = Real(999961)
        val small: Real = 1 / Real(999983)
        val product = new RealProduct(Seq(small, large))
        product.evaluate(4 dp) shouldBe BigDecimal("1.0000")
        product.evaluate(5 dp) shouldBe BigDecimal("0.99998")
    }

}
