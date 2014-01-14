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

    val ONE: IntegerNumber = 1
    val TWO: IntegerNumber = 2
    val THREE: IntegerNumber = 3
    val FOUR: IntegerNumber = 4
    val FIVE: IntegerNumber = 5

    "4/1" should "reduce" in {
        var r: RealNumber = FOUR / 1
        r shouldBe (FOUR)
    }

    "4/2" should "reduce" in {
        val r: RealNumber = FOUR / TWO
        r shouldBe (TWO)
    }

    "2/4" should "reduce" in {
        val r: RealNumber = TWO / FOUR
        r.evaluate(2 dp).toString shouldBe ("0.50")
        r shouldBe (ONE / TWO)
    }

    behavior of "4/5"

    it should "multiply" in {
        val r: RealNumber = FOUR / FIVE
        r.evaluate(2 dp).toString shouldBe ("0.80")
        r * 5 shouldBe (FOUR)
    }

    it should "evaluate" in {
        val r: RealNumber = FOUR / FIVE
    }

    behavior of "1/3"

    it should "evaluate" in {
        val r = ONE / THREE
        r.evaluate(4 dp).toString shouldBe ("0.3333")
    }

}
