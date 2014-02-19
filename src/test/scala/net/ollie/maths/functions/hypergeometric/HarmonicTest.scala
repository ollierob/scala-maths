package net.ollie.maths.functions.hypergeometric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.constants.One
import net.ollie.maths.numbers.Precision._

/**
 * Created by Ollie on 17/02/14.
 */
class HarmonicTest extends FlatSpec with Matchers {

    "Harmonic(1)" should "equal 1" in {
        val h = Harmonic(One)
        h.evaluate(4 dp) shouldBe BigDecimal("1.0000")
        h shouldBe One
    }

}
