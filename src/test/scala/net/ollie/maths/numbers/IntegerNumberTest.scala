package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 02/01/14.
 */
@RunWith(classOf[JUnitRunner])
class IntegerNumberTest extends FlatSpec with Matchers {

    behavior of "2"

    it should "evaluate" in {
        IntegerNumber(2).evaluate shouldBe (BigInt(2))
    }

    it should "negate" in {
        val i = IntegerNumber(2)
        (-i).evaluate shouldBe (BigInt(-2))
        (-i) shouldBe (IntegerNumber(-2))
        (-(-i)) shouldBe (i)
    }

    it should "add to another integer" in {
        val i = IntegerNumber(2)
        (i + 0) shouldBe (i)
        (i + 1) shouldBe (IntegerNumber(3))
        (i + i) shouldBe (IntegerNumber(4))
        (i ?+ i) shouldBe (Some(IntegerNumber(4)))
    }

}
