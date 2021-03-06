package net.ollie.maths.functions.angular

import net.ollie.maths.Variable
import net.ollie.maths.functions.angular.Angle._
import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.{Pi, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 03/01/14.
 */
class SinTest extends FlatSpec with Matchers {

    val x = Variable("x")
    val sin = Sin(x)

    {
        behavior of "Sin(x)"

        val x = Variable("x")
        val sin = Sin(x)

        it should "not be empty" in {
            sin.isEmpty shouldBe (false)
        }

        it should "replace" in {
            sin.replace(x, 5).toConstant shouldBe Some(Sin(5))
        }

        it should "differentiate" in {
            sin.df(x) shouldBe Cos(x)
        }

    }

    behavior of "sin(0)"

    it should "replace" in {
        sin.replace(x, 0).toConstant shouldBe Some(Zero)
    }

    "Sin(1)" should "evaluate" in {
        Sin(1 radian).evaluate(4 dp) shouldBe BigDecimal("0.8415")
    }

    "Sin(2)" should "evaluate" in {
        val s = Sin(2 radians)
        s.isEmpty shouldBe false
        s.evaluate(4 dp) shouldBe BigDecimal("0.9093")
    }

    "Sin(Pi)" should "be empty" in {
        Sin(Pi).isEmpty shouldBe true
    }

    "Sin(2 Pi)" should "be empty" in {
        val s = Sin(2 * Pi)
        s.evaluate(4 dp) shouldBe BigDecimal("0.0000")
        s.isEmpty shouldBe true
    }

}
