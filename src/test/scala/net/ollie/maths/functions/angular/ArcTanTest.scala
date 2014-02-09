package net.ollie.maths.functions.angular

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.IntegerFraction
import net.ollie.maths.numbers.Precision._
import net.ollie.maths.Variable
import net.ollie.maths.numbers.constants.{Zero, One, Half}

/**
 * Created by Ollie on 09/02/14.
 */
class ArcTanTest extends FlatSpec with Matchers {

    val x = Variable("x")

    behavior of "ArcTan(x)"

    it should "differentiate" in {
        val d = ArcTan(x).df(x)
        d.replace(x, 0).toConstant shouldBe Some(One)
        d.replace(x, 1).toConstant shouldBe Some(Half)
        d.replace(x, 2).toConstant shouldBe Some(IntegerFraction(1, 5))
    }

    "ArcTan(0)" should "be zero" in {
        ArcTan(0) shouldBe Zero
        ArcTan(0).isEmpty shouldBe true
    }

    "ArcTan(1/5)" should "evaluate" in {
        ArcTan(IntegerFraction(1, 5)).evaluate(8 dp) shouldBe BigDecimal("0.19739556")
    }

    "4 ArcTan(1/5) - ArcTan(1/239)" should "be Pi/4" in {
        val v = (4 * ArcTan(IntegerFraction(1, 5))) - ArcTan(IntegerFraction(1, 239))
        v.evaluate(4 dp) shouldBe BigDecimal("0.7854")
    }

}
