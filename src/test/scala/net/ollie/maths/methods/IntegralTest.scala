package net.ollie.maths.methods

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.{Precision, Real}
import Precision._
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 19/01/14.
 */
class IntegralTest extends FlatSpec with Matchers {

    "Integral of x degree 0 to 1" should "equal 1/2" in {
        val integral: Real = Integral(x => x, Zero, One)(SimpsonsIntegrationMethod)
        integral.evaluate(4 dp) shouldBe BigDecimal("0.5000")
    }

    "Integral of x^2 degree -1 to 1" should "equal " in {
        val integral = Integral(x => x ^ 2, -1, 1)(SimpsonsIntegrationMethod)
        integral.evaluate(4 dp) shouldBe BigDecimal("0.6667")
    }

}
