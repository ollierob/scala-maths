package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.constants.{MinusOne, Pi}
import net.ollie.maths.numbers.Precision._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import net.ollie.maths.numbers.complex.ImaginaryUnit
import net.ollie.maths.numbers.{Integer, Real}

/**
 * Created by Ollie on 13/02/14.
 */
@RunWith(classOf[JUnitRunner])
class PrincipalRootTest extends FlatSpec with Matchers {

    "Principal 3rd root of Pi" should "evaluate" in {
        val root = PrincipalRoot(Pi, 3)
        root.evaluate(4 dp) shouldBe BigDecimal("1.4646")
    }

    behavior of "Principal root of -1"

    {

        it should "be i if degree is even" in {
            PrincipalRoot(MinusOne, 2) shouldBe ImaginaryUnit
            PrincipalRoot(MinusOne, 4) shouldBe ImaginaryUnit
        }

        it should "be -1 if degree is odd" in {
            PrincipalRoot(MinusOne, 1) shouldBe MinusOne
            PrincipalRoot(MinusOne, 3) shouldBe MinusOne
            PrincipalRoot(MinusOne, 5) shouldBe MinusOne
        }

    }

    behavior of "Principal root of 8"

    {

        it should "have cube root" in {
            val root: Real = PrincipalRoot(8, 3)
            root shouldBe Integer(2)
        }

        it should "have forth root" in {
            val root: Real = PrincipalRoot(8, 4)
            root.evaluate(4 dp) shouldBe BigDecimal("1.6818")
        }

    }

}
