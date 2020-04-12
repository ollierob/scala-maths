package net.ollie.maths.numbers.constants

import net.ollie.maths.numbers.Irrational
import net.ollie.maths.numbers.Precision._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 14/01/14.
 */
class PiTest extends FlatSpec with Matchers {

    behavior of "Pi"

    {

        it should "evaluate to 8 dp" in {
            Pi.evaluate(8 dp) shouldBe BigDecimal("3.14159265")
        }

        it should "evaluate to 8 sf" in {
            Pi.evaluate(8 sf) shouldBe BigDecimal("3.1415927")
        }

        it should "not be empty" in {
            Pi shouldNot be(Zero)
        }

    }

    behavior of "Pi * 2"

    {

        it should "evaluate" in {
            2 * Pi evaluate (4 dp) shouldBe BigDecimal("6.2832")
        }

        it should "be irrational" in {
            Irrational.is(2 * Pi) shouldBe true
        }

    }

    behavior of "Pi / 2"

    {

        it should "evaluate to 8 dp" in {
            Pi / 2 evaluate (8 dp) shouldBe BigDecimal("1.57079633")
        }

        it should "evaluate to 8 sf" in {
            Pi / 2 evaluate (8 sf) shouldBe BigDecimal("1.5707963")
        }

        it should "not be empty" in {
            Pi / 2 shouldNot be(Zero)
        }

    }

}
