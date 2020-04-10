package net.ollie.maths.functions.angular

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.{One, Pi}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 12/01/14.
 */
class CosTest extends FlatSpec with Matchers {

    //
    //    {
    //
    //        behavior of "Cos(x)"
    //
    //        val x = Variable("x")
    //        val cos = Cos(x)
    //
    //        it should "replace" in {
    //            cos.replace(x, Zero) shouldBe One
    //        }
    //
    //    }
    //
    //    "Cos(0)" should "equal 1" in {
    //        Cos(Zero) shouldBe (One)
    //    }
    //
    //    "Cos(Pi)" should "be -1" in {
    //        val c = Cos(Pi)
    //        c.evaluate(4 dp) shouldBe BigDecimal("-1.0000")
    //        c shouldBe MinusOne
    //    }

    "Cos(2 Pi)" should "be 1" in {
        val c = Cos(2 * Pi)
        c.evaluate(4 dp) shouldBe BigDecimal("1.0000")
        c shouldBe One
    }

}
