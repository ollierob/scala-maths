package net.ollie.maths.functions.polynomial.roots

import net.ollie.maths.Variable
import net.ollie.maths.functions.polynomial.Polynomial
import net.ollie.maths.numbers.Precision._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DurandKernerTest extends AnyFlatSpec with Matchers {

    val x = Variable("x")

    behavior of "x^3 - 3x^2 + 3x - 5 = 0"

    {

        val poly = Polynomial(x, Seq(-5, 3, 3, 1))

        it should "solve to 1dp" in {

            val roots = new DurandKerner(i => (i, i + 1)).roots(poly, 1 dp)

        }

    }

}
