package net.ollie.maths.functions.angular

import net.ollie.maths.numbers.{IntegerFraction, Real}
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.Pi

/**
 * Created by Ollie on 24/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ArcCosTest extends FlatSpec with Matchers {

    behavior of "ArcCos(x)"

    it should "evaluate at x = 0.5" in {
        val r = ArcCos(Real(0.5))
        r.evaluate(4 dp) shouldBe BigDecimal("1.0472")
    }

    "ArcCos(3/5)" should "evaluate" in {
        ArcCos(IntegerFraction(3, 5)).evaluate(4 dp) shouldBe BigDecimal("0.9273")
    }

    "ArcCos(3/5) + ArcCos(4/5)" should "equal Pi/2" in {
        val r = ArcCos(IntegerFraction(3, 5)) + ArcCos(IntegerFraction(4, 5))
        r.evaluate(4 dp) shouldBe BigDecimal("1.5708")
        r shouldBe Pi / 2
    }

}
