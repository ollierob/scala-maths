package net.ollie.maths.functions.angular

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 22/01/14.
 */
class ArcSinTest extends FlatSpec with Matchers {

    "ArcSin(0)" should "be 0" in {
        ArcSin(Zero) shouldBe Zero
    }

    "ArcSin(0.5)" should "evaluate" in {
        val a = ArcSin(0.5)
        a.evaluate(4 dp) shouldBe BigDecimal("0.5236")
    }

    "ArcSin(1)" should "be 1" in {
        ArcSin(One) shouldBe One
    }

}
