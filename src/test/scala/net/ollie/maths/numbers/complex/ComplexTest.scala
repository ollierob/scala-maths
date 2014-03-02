package net.ollie.maths.numbers.complex

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.Integer
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ComplexTest extends FlatSpec with Matchers {

    "1" should "widen" in {
        val z: Complex = One
        z shouldBe Complex(One, Zero)
    }

    behavior of "2 + 3i"

    {

        val z = Complex(2, 3)

        it should "have re() = 2" in {
            z.re shouldBe Integer(2)
        }

        it should "have im() = 3" in {
            z.im shouldBe Integer(3)
            z.unre shouldBe Integer(3)
        }

        it should "not be empty" in {
            z.isEmpty shouldBe false
        }

        it should "not equal 3 + 2i" in {
            z == Complex(3, 2) shouldBe false
            z.equals(Complex(3, 2)) shouldBe false
        }

        it should "abs" in {
            z.abs shouldBe PositiveSquareRoot(13)
        }

        it should "invert" in {
            z.inverse shouldBe (Complex(Integer(2) / Integer(13), Integer(-3) / Integer(13)))
        }

        it should "multiply by 5" in {
            z * 5 shouldBe Complex(10, 15)
            z * Complex(5, 0) shouldBe Complex(10, 15)
            Complex(5, 0) * z shouldBe Complex(10, 15)
            5 * z shouldBe Complex(10, 15)
        }

        it should "divide by 5" in {
            z / Integer(5) shouldBe Complex(Integer(2) / Integer(5), Integer(3) / Integer(5))
            z / Complex(5, 0) shouldBe Complex(Integer(2) / Integer(5), Integer(3) / Integer(5))
        }

        it should "multiply by 5+7i" in {
            val z2 = Complex(5, 7)
            z * z2 shouldBe Complex(-11, 29)
            z2 * z shouldBe Complex(-11, 29)
        }

        it should "divide by 5 + 7i" in {
            val z2 = Complex(5, 7)
            z / z2 shouldBe (Complex(Integer(31) / Integer(74), Integer(1) / Integer(74)))
        }

    }

}
