package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.Zero
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NthDegreePolynomialTest extends AnyFlatSpec with Matchers {

    val x = Variable("x")

    behavior of "5x^3 + 4x^2 + 3x + 2"

    {

        val poly = NthDegreePolynomial(x, Seq(2, 3, 4, 5));

        it should "have coefficients" in {
            poly.coefficient(0) shouldBe Natural(2)
            poly.coefficient(1) shouldBe Natural(3)
            poly.coefficient(2) shouldBe Natural(4)
            poly.coefficient(3) shouldBe Natural(5)
            poly.coefficient(4) shouldBe Zero
        }

        it should "differentiate" in {
            poly.derivative shouldBe NthDegreePolynomial(x, Seq(3, 8, 15))
        }

        it should "substitute" in {
            poly.replace(Natural(1)) shouldBe Natural(14)
            poly.replace(Natural(2)) shouldBe Natural(64)
        }

    }

}
