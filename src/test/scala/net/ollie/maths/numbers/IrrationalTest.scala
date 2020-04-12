package net.ollie.maths.numbers

import net.ollie.maths.numbers.constants.Pi
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IrrationalTest extends AnyFlatSpec with Matchers {

    behavior of "pi"

    {

        it should "be irrational" in {

            Pi.is(Irrational) shouldBe true

        }

        it should "have irrational products" in {

            (Pi * -1).is(Irrational) shouldBe true
            (Pi * 2).is(Irrational) shouldBe true

        }

    }

}
