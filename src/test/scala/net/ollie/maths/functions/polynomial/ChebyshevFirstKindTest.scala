package net.ollie.maths.functions.polynomial

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Zero, NaturalNumber, One}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 18/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ChebyshevFirstKindTest extends FlatSpec with Matchers {

    val x = Variable("x")

    behavior of "T(2)(x)"

    {

        val f = ChebyshevFirstKind(2)(x) //2x^2 -1

        it should "evaluate" in {
            f.replace(x, Zero).toConstant shouldBe Some(-One)
            f.replace(x, One).toConstant shouldBe Some(One)
        }

        it should "differentiate to 4x" in {
            val df = f.df(x)
            df.replace(x, Zero).toConstant shouldBe Some(Zero)
            println(f.replace(x, Variable("y")))
            println(df)
            df.replace(x, One).toConstant shouldBe Some(NaturalNumber(4))
        }

    }

}
