package net.ollie.maths.functions.polynomial

import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Integer, One, Zero}
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
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
            df.replace(x, One).toConstant shouldBe Some(Integer(4))
        }

    }

    behavior of "T(3)(x)"

    {

        val f = ChebyshevFirstKind(3)(x) //4x^3 - 3x

        it should "evaluate" in {
            f.replace(x, One).toConstant shouldBe Some(One)
        }

        it should "differentiate to 12x^2 - 3" in {
            val df = f.df(x)
            df.replace(x, Zero).toConstant shouldBe Some(Integer(-3))
            df.replace(x, One).toConstant shouldBe Some(Integer(9))
        }

    }

}
