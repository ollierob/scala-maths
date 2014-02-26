package net.ollie.maths.functions.numeric

import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.numbers.constants.{Pi, Two}
import net.ollie.maths.numbers.{Integer, IntegerFraction}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 26/02/14.
 */
@RunWith(classOf[JUnitRunner])
class IntegerPartTest extends FlatSpec with Matchers {

    "Integer part of 5" should "be 5" in {
        IntegerPart(Integer(5)) shouldBe Integer(5)
    }

    "Integer part of 7 / 3" should "be 2" in {
        IntegerPart(IntegerFraction(7, 3)) shouldBe Two
    }

    "Integer part of Pi" should "be 3" in {
        IntegerPart(Pi) shouldBe Integer(3)
    }

}
