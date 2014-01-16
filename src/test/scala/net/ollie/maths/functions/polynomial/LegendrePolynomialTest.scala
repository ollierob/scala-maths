package net.ollie.maths.functions.polynomial

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Zero, One}
import net.ollie.maths.numbers.real.Pi

/**
 * Created by Ollie on 09/01/14.
 */
class LegendrePolynomialTest extends FlatSpec with Matchers {

    val x = Variable("x")

    behavior of "P(1)(x)"

    {

        val p1 = LegendrePolynomial(1, x)

        it should "be x" in {
            LegendrePolynomial(1, Zero) shouldBe (Zero)
            LegendrePolynomial(1, Pi) shouldBe (Pi)
            LegendrePolynomial(1, x).replace(x, Pi) shouldBe (Pi)
        }

        it should "differentiate" in {
            LegendrePolynomial(1, x).df(x) shouldBe (One)
        }

    }

    behavior of "P(2)(x)"

    {

        val p2 = LegendrePolynomial(2, x)

        it should "(3x^2 - 1)/2" in {
            p2.replace(x, Zero) shouldBe (-One / 2)
            p2.replace(x, One) shouldBe (One)
        }

        it should "differentiate" in {
            val df = p2.df(x)
        }

    }

}
