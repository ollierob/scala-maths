package net.ollie.maths.functions.polylogarithmic

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.{Half, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 19/02/14.
 */
class RiemannZetaTest extends FlatSpec with Matchers {

    "Zeta(0)" should "be -1/2" in {
        RiemannZeta(0) shouldBe -Half
    }

    "Zeta(2)" should "be Pi^2 / 6" in {
        RiemannZeta(2).evaluate(4 dp) shouldBe BigDecimal("1.6449")
    }

    "Zeta(3)" should "be 0" in {
        RiemannZeta(3) shouldBe Zero
    }

    "Zeta(4)" should "be Pi^4 / 90" in {
        RiemannZeta(4).evaluate(4 dp) shouldBe BigDecimal("1.0823")
    }

}
