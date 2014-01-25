package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 04/01/14.
 */
@RunWith(classOf[JUnitRunner])
class IntegerFractionTest extends FlatSpec with Matchers {

    import Precision._

    val ONE: Integer = 1
    val TWO: Integer = 2
    val THREE: Integer = 3
    val FOUR: Integer = 4
    val FIVE: Integer = 5

    "4/1" should "reduce" in {
        var r: Real = FOUR / 1
        r shouldBe (FOUR)
    }

    "4/2" should "reduce" in {
        val r: Real = FOUR / TWO
        r shouldBe (TWO)
    }

    "2/4" should "reduce" in {
        val r: Real = TWO / FOUR
        r.evaluate(2 dp).toString shouldBe ("0.50")
        r shouldBe (ONE / TWO)
    }

    behavior of "4/5"

    it should "multiply" in {
        val r: Real = FOUR / FIVE
        r.evaluate(2 dp).toString shouldBe ("0.80")
        r * 5 shouldBe (FOUR)
    }

    it should "evaluate" in {
        val r: Real = FOUR / FIVE
    }

    behavior of "1/3"

    it should "evaluate" in {
        val r = ONE / THREE
        r.evaluate(4 dp).toString shouldBe ("0.3333")
    }

    "-4 / 2" should "equal -2" in {
        ((-FOUR) / TWO).evaluate(4 dp) shouldBe (BigDecimal(-2))
    }

    "-4 / -2" should "equal 2" in {
        ((-FOUR) / (-TWO)).evaluate(4 dp) shouldBe (BigDecimal(2))
    }

    "1/3 + 1/5" should "equal 8/15" in {
        IntegerFraction(ONE, THREE) + IntegerFraction(ONE, FIVE) shouldBe (IntegerFraction(8, 15))
    }

}
