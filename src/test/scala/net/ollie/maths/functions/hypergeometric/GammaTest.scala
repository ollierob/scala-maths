package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.numbers.Real
import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.numbers.Precision._

/**
 * Created by Ollie on 21/01/14.
 */
class GammaTest extends FlatSpec with Matchers {

    "Gamma(4.5)" should "evaluate" in {
        val g = Gamma(4.5)
        g.evaluate(1 dp) shouldBe BigDecimal("11.6") //FIXME
    }

}
