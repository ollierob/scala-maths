package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.numbers.Natural
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PolynomialTest extends AnyFlatSpec with Matchers {

    val x = Variable("x")

    behavior of "5x^3 + 4x^2 + 3x + 2"

    {

        val poly = Polynomial(x, Seq(2, 3, 4, 5))

        it should "equal" in {
            poly shouldBe new UnivariateNthDegreePolynomial(x, Seq(2, 3, 4, 5))
        }

        it should "replace" in {
            poly.replace(1) shouldBe Natural(5 + 4 + 3 + 2)
        }

    }

}
