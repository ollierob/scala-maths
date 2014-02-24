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

    it should "negate" in {
        val r: Rational = FOUR / FIVE
        -r shouldBe (-FOUR) / FIVE
    }

    it should "invert" in {
        (FOUR / FIVE).inverse shouldBe FIVE / FOUR
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

    "3/5" should "evaluate" in {
        IntegerFraction(3, 5).evaluate(4 dp) shouldBe BigDecimal("0.6000")
    }

    "2/3 + 5/7" should "equal 29/21" in {
        val r: Rational = IntegerFraction(2, 3) + IntegerFraction(5, 7)
        r shouldBe IntegerFraction(29, 21)
    }

    "1 / 99991" should "evaluate" in {
        val fraction = 1 / Integer(99991)
        fraction.evaluate(4 dp) shouldBe BigDecimal("0.0000")
        fraction.evaluate(5 dp) shouldBe BigDecimal("0.00001")
        fraction.approximatelyEvaluate(4 dp) shouldBe BigDecimal("0.0000")
        fraction.approximatelyEvaluate(5 dp) shouldBe BigDecimal("0.00001")
    }

    "3 / 2" should "evaluate" in {
        IntegerFraction(3, 2).evaluate(4 dp) shouldBe BigDecimal("1.5000")
        Integer(3) / Integer(2) shouldBe IntegerFraction(3, 2)
    }

}
