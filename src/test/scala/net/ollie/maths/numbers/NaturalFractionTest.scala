package net.ollie.maths.numbers

import net.ollie.maths.numbers.Precision._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 16/02/14.
 */
class NaturalFractionTest extends FlatSpec with Matchers {

    "1/10!" should "evaluate" in {
        val tenFactorial = Natural(10) !
        val inverse = 1 / tenFactorial
        inverse.evaluate(8 dp) shouldBe BigDecimal("0.00000028")
        (-inverse).evaluate(8 dp) shouldBe BigDecimal("-0.00000028")
    }

    "large / large" should "evaluate" in {
        val numerator: Natural = 99989
        val denominator: Natural = 99991
        val fraction = numerator / denominator
        fraction.evaluate(2 dp) shouldBe BigDecimal("1.00")
        fraction.evaluate(5 dp) shouldBe BigDecimal("0.99998")
    }

}
