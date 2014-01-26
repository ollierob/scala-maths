package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 26/01/14.
 */
class AssociatedLegendrePolynomialTest extends FlatSpec with Matchers {

    val x = Variable("x")

    "P(l, m) with |m| > l" should "be empty" in {
        AssociatedLegendrePolynomial(0, 1, x).isEmpty shouldBe true
        AssociatedLegendrePolynomial(1, 2, x).isEmpty shouldBe true
        AssociatedLegendrePolynomial(2, 3, x).isEmpty shouldBe true
    }

    "P(l, m) with l < 0" should "express" in {
        AssociatedLegendrePolynomial(-1, 0, x) shouldBe AssociatedLegendrePolynomial(0, 0, x)
        AssociatedLegendrePolynomial(-2, 1, x) shouldBe AssociatedLegendrePolynomial(1, 1, x)
        AssociatedLegendrePolynomial(-2, 2, x).isEmpty shouldBe true
    }

}
