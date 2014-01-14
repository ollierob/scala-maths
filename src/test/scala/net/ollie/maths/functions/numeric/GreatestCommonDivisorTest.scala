package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.IntegerNumber
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * Created by Ollie on 04/01/14.
 */
@RunWith(classOf[JUnitRunner])
class GreatestCommonDivisorTest extends FlatSpec with Matchers {

    "gcd(48, 18)" should "be 6" in {
        GreatestCommonDivisor(48, 18) shouldBe (IntegerNumber(6))
    }

    "gcd(54, 24)" should "be 6" in {
        GreatestCommonDivisor(54, 24) shouldBe (IntegerNumber(6))
    }

    "gcd(7, 5)" should "be 1" in {
        GreatestCommonDivisor(7, 5) shouldBe (IntegerNumber(1))
    }

}
