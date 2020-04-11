package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SingleVariableRealQuadraticTest extends AnyFlatSpec with Matchers {

    val x = Variable("x")

    behavior of "P(x) = 5x^2 + 6x + 1"

    {

        val poly = QuadraticPolynomial(x, 5, 6, 1)

        it should "string" in {
            poly.toString shouldBe "5.x^2 + 6.x + 1"
        }

        it should "roots" in {

            val roots = poly.roots
            roots.principal shouldBe 5

        }

    }

}
