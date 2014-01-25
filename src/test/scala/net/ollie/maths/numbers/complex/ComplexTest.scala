package net.ollie.maths.numbers.complex

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.Integer
import net.ollie.maths.functions.numeric.PositiveSquareRoot

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ComplexTest extends FlatSpec with Matchers {

    behavior of "2 + 3i"

    {

        val z = Complex(2, 3)

        it should "have re() = 2" in {
            z.re shouldBe Integer(2)
        }

        it should "have im().re() = 3" in {
            z.im.coefficient shouldBe Integer(3)
        }

        it should "not be empty" in {
            z.isEmpty shouldBe false
        }

        it should "abs" in {
            z.abs shouldBe PositiveSquareRoot(13)
        }

        it should "invert" in {
            z.inverse shouldBe (Complex(Integer(2) / Integer(13), Integer(-3) / Integer(13)))
        }

        it should "divide by 5 + 7i" in {
            val z2 = Complex(5, 7)
            z / z2 shouldBe (Complex(Integer(31) / Integer(74), Integer(1) / Integer(74)))
            println(z / z2)
        }

    }

}
