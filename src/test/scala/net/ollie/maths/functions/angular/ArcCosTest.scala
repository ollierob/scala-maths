package net.ollie.maths.functions.angular

import net.ollie.maths.numbers.RealNumber
import net.ollie.maths.numbers.Precision._
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 24/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ArcCosTest extends FlatSpec with Matchers {

    behavior of "ArcCos(x)"

    it should "evaluate at x = 0.5" in {
        val r = ArcCos(RealNumber(0.5))
        r.evaluate(4 dp) shouldBe BigDecimal("1.0472")
    }

}
