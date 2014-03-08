package net.ollie.maths.functions.polynomial

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.{IntegerFraction, Real}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 08/03/14.
 */
@RunWith(classOf[JUnitRunner])
class EulerPolynomialTest extends FlatSpec with Matchers {

    "Euler(3, 3)" should "be 27/2 + 1/4" in {
        val r: Real = EulerPolynomial(3, 3)
        r shouldBe IntegerFraction(55, 4)
    }

}
