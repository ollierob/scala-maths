package net.ollie.maths.numbers

import net.ollie.maths.numbers.constants.Pi
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IrrationalTest extends AnyFlatSpec with Matchers {

    behavior of "pi"

    {

        it should "be irrational" in {

            Irrational.is(Pi) shouldBe true

        }

        it should "have irrational products" in {

            Irrational.is(Pi * -1) shouldBe true
            Irrational.is(Pi * 2) shouldBe true

        }

    }

}
