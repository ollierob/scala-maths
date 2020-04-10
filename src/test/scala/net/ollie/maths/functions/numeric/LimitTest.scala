package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 03/03/14.
 */
class LimitTest extends FlatSpec with Matchers {

    "Lim(x as x->0+)" should "be 0" in {
        val limit = Limit.fromAbove(x => x, Zero)
        limit.evaluate(4 dp) shouldBe BigDecimal("0.0000")
    }

    "Lim(x as x->0-)" should "be 0" in {
        val limit = Limit.fromBelow(x => x, Zero)
        limit.evaluate(4 dp) shouldBe BigDecimal("0.0000")
    }

    "Lim((x^2-1)/(x-1) as x->1+)" should "be 2" in {
        val limit = Limit.fromAbove(x => ((x ^ 2) - 1) / (x - 1), One)
        limit.evaluate(4 dp) shouldBe BigDecimal("2.0000")
    }

}
