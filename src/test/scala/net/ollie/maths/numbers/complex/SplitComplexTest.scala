package net.ollie.maths.numbers.complex

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Zero

/**
 * Created by Ollie on 05/02/14.
 */
class SplitComplexTest extends FlatSpec with Matchers {

    behavior of "1 + j"

    {

        val s = SplitComplex(1, 1)

        it should "equal itself" in {
            s == s shouldBe true
        }

        it should "conjugate" in {
            s.conjugate shouldBe SplitComplex(1, -1)
        }

        it should "have a zero divisor" in {
            val s2 = s.conjugate
            s * s2 shouldBe Zero
            s2 * s shouldBe Zero
        }

    }

}
