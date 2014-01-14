package net.ollie.maths.functions.polynomial

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Zero, One}
import net.ollie.maths.numbers.real.Pi

/**
 * Created by Ollie on 09/01/14.
 */
class LegendrePolynomialTest extends FlatSpec with Matchers {

    val x = new Variable("x")

    behavior of "P(1)"

    it should "be x" in {
        LegendrePolynomial(1, Zero) shouldBe (Zero)
        LegendrePolynomial(1, Pi) shouldBe (Pi)
        LegendrePolynomial(1, x).replace(x, Pi) shouldBe (Pi)
    }

    it should "differentiate" in {
        LegendrePolynomial(1, x).df(x) shouldBe (One)
    }

    behavior of "P(2)"

    it should "(3x^2 - 1)/2" in {
        LegendrePolynomial(2, Zero) shouldBe (-One / 2)
        LegendrePolynomial(2, One) shouldBe (One)
    }

    it should "differentiate" in {
        println(LegendrePolynomial(2, x).df(x))
    }


}
