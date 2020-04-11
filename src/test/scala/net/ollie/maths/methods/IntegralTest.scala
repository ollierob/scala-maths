package net.ollie.maths.methods

import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.numbers.{Precision, Real}
import Precision._
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.Variable
import net.ollie.maths.expressions.Univariate

/**
 * Created by Ollie on 19/01/14.
 */
class IntegralTest extends FlatSpec with Matchers {

    "Integral of x degree 0 to 1" should "equal 1/2" in {
        def fn(x: Variable): Univariate = x
        val integral: Real = Integrate(fn _, Zero, One)(SimpsonsIntegrationMethod)
        integral.evaluate(4 dp) shouldBe BigDecimal("0.5000")
    }

    "Integral of x^2 degree -1 to 1" should "equal " in {
        def fn(x: Variable): Univariate = x ^ 2
        val integral = Integrate(fn _, -1, 1)(SimpsonsIntegrationMethod)
        integral.evaluate(4 dp) shouldBe BigDecimal("0.6667")
    }

}
