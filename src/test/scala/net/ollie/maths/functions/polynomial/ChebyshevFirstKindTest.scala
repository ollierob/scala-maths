package net.ollie.maths.functions.polynomial

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import net.ollie.maths.numbers.One

/**
 * Created by Ollie on 18/01/14.
 */
class ChebyshevFirstKindTest extends FlatSpec with Matchers {

    val x = Variable("x")

    behavior of "T(2)(x)"

    {

        val t2 = ChebyshevFirstKind(2)(x)

        it should "evaluate" in {
            t2.replace(x, One).toConstant shouldBe (Some(One))
        }

    }

}
