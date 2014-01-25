package net.ollie.maths.functions.polynomial

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import net.ollie.maths.numbers.One
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

        val t2 = ChebyshevFirstKind(2)(x) //2x^2 -1

        it should "evaluate" in {
            println(t2.replace(x, x))
            t2.replace(x, One).toConstant shouldBe Some(One)
        }

        //        it should "differentiate to 4x" in {
        //            val df = t2.df(x)
        //            df.replace(x, One).toConstant shouldBe NaturalNumber(4)
        //        }

    }

}
