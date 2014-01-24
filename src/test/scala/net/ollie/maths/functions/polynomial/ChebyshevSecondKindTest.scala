package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Zero, NaturalNumber, One}
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 19/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ChebyshevSecondKindTest extends FlatSpec with Matchers {

    val x = Variable("x")

    behavior of "U(2, x)"

    {

        val u2 = ChebyshevSecondKind(2)(x)

        it should "evaluate" in {
            u2.replace(x, One).toConstant shouldBe Some(NaturalNumber(4 - 1))
        }

        it should "not be empty" in {
            u2.isEmpty shouldBe false
        }

    }

    behavior of "U(3, x)"

    {

        val u3 = ChebyshevSecondKind(3)(x)

        it should "evaluate" in {
            u3.replace(x, 0).toConstant shouldBe Some(Zero)
            u3.replace(x, 1).toConstant shouldBe Some(NaturalNumber(8 - 4))
        }

    }

}
