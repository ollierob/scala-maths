package net.ollie.maths

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.functions.numeric.SquareRoot
import net.ollie.maths.numbers.One

/**
 * Created by Ollie on 24/01/14.
 */
class ExpressionPowerTest extends FlatSpec with Matchers {

    val x = Variable("x")

    behavior of "1 / Sqrt(x)"

    it should "differentiate" in {
        val f = 1 / SquareRoot(x)
        val df = f.df(x)
        df.isEmpty shouldBe false
        df.replace(x, One).toConstant shouldBe Some(-One / 2)
    }

    behavior of "x^x"

    it should "differentiate" in {
        val f = x ^ x
        val df = f.df(x)
        println(df)
        df.isEmpty shouldBe false
        println(df.replace(x, One))
        df.replace(x, One).toConstant shouldBe Some(One)
    }

}
