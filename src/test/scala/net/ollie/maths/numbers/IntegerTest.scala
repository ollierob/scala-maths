package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 02/01/14.
 */
@RunWith(classOf[JUnitRunner])
class IntegerTest extends FlatSpec with Matchers {

    behavior of "1"

    it should "not be even" in {
        One.isEven shouldBe (false)
    }

    behavior of "2"

    it should "evaluate" in {
        Integer(2).evaluate shouldBe (BigInt(2))
    }

    it should "be even" in {
        Integer(2).isEven shouldBe (true)
    }

    it should "negate" in {
        val i = Integer(2)
        (-i).evaluate shouldBe (BigInt(-2))
        (-i) shouldBe (Integer(-2))
        (-(-i)) shouldBe (i)
    }

    it should "add to another integer" in {
        val i = Integer(2)
        (i + 0) shouldBe (i)
        (i + 1) shouldBe (Integer(3))
        (i + i) shouldBe (Integer(4))
        (i ?+ i) shouldBe (Some(Integer(4)))
    }

    behavior of "-2"

    it should "not be positive" in {
        Integer(-2).isStrictlyPositive shouldBe false
    }

    it should "be even" in {
        Integer(-2).isEven shouldBe true
    }

}
