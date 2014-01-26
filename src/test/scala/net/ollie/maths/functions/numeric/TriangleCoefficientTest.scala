package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Precision._

/**
 * Created by Ollie on 26/01/14.
 */
class TriangleCoefficientTest extends FlatSpec with Matchers {

    "Triangle(1, 1, 2)" should "evaluate" in {
        val r = TriangleCoefficient(1, 1, 2) //(0! * 2! * 2!) / (5!) = 4/120
        r.isEmpty shouldBe false
        r.evaluate(4 dp) shouldBe BigDecimal("0.0333")
    }

}
