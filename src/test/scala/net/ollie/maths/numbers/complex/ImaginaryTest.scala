package net.ollie.maths.numbers.complex

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.{Zero, One}
import net.ollie.maths.numbers.Precision._

/**
 * Created by Ollie on 12/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ImaginaryTest extends FlatSpec with Matchers {

    behavior of "i"

    {

        val i = ImaginaryUnit

        it should "be equal to itself" in {
            i == i shouldBe (true)
            i == Complex(0, 1) shouldBe (true)
            Complex(0, 1) == i shouldBe (true)
        }

        it should "not be equal to something else" in {
            val j = Imaginary(2)
            i == j shouldBe (false)
            j == i shouldBe (false)
        }

        it should "have re() = 0" in {
            i.re shouldBe Zero
        }

        it should "have im() = 1" in {
            i.im shouldBe One
        }

        it should "exponentiate to integer" in {
            i ^ 0 shouldBe One
            i ^ 1 shouldBe i
            i ^ 2 shouldBe -One
            i ^ 3 shouldBe -i
            i ^ 4 shouldBe One
        }

        it should "exponentiate to imaginary" in {
            val power: Complex = (i ^ i).principal
            power.toReal.isDefined shouldBe true
            power.toReal.get.evaluate(4 dp) shouldBe BigDecimal("0.2079")
        }

    }

}
