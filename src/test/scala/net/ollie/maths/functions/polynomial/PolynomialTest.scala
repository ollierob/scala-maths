package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PolynomialTest extends AnyFlatSpec with Matchers {

    val x = Variable("x")

    behavior of "5x^3 + 4x^2 + 3x + 2"

    {

        val poly = Polynomial(x, Seq(2, 3, 4, 5))
        poly shouldBe new UnivariateNthDegreePolynomial(x, Seq(2, 3, 4, 5))

    }

}
