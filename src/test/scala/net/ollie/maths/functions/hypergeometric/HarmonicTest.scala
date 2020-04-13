package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.One
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Created by Ollie on 17/02/14.
 */
class HarmonicTest extends AnyFlatSpec with Matchers {

    behavior of "Harmonic(1)"

    {

        it should "equal 1" in {
            val h = Harmonic(One)
            h.evaluate(4 dp) shouldBe BigDecimal("1.0000")
            h shouldBe One
        }

    }

}
