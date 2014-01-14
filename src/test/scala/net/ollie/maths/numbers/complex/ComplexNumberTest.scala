package net.ollie.maths.numbers.complex

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.IntegerFraction

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ComplexNumberTest extends FlatSpec with Matchers {

    behavior of "2 + 3i"

    {

        val z = ComplexNumber(2, 3)

        it should "invert" in {
            z.inverse shouldBe (ComplexNumber(IntegerFraction(2, 13), IntegerFraction(-3, 13)))
        }

        it should "divide by 5 + 7i" in {
            val z2 = ComplexNumber(5, 7)
            z / z2 shouldBe (ComplexNumber(IntegerFraction(31, 74), IntegerFraction(1, 74)))
        }

    }

}
