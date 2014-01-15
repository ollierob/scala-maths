package net.ollie.maths.numbers.complex

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.IntegerNumber

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ComplexNumberTest extends FlatSpec with Matchers {

    behavior of "2 + 3i"

    {

        val z = ComplexNumber(2, 3)

        it should "invert" in {
            z.inverse shouldBe (ComplexNumber(IntegerNumber(2) / IntegerNumber(13), IntegerNumber(-3) / IntegerNumber(13)))
        }

        it should "divide by 5 + 7i" in {
            val z2 = ComplexNumber(5, 7)
            z / z2 shouldBe (ComplexNumber(IntegerNumber(31) / IntegerNumber(74), IntegerNumber(1) / IntegerNumber(74)))
            println(z / z2)
        }

    }

}
