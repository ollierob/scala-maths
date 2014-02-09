package net.ollie.maths.functions.hypergeometric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 22/01/14.
 */
@RunWith(classOf[JUnitRunner])
class BetaTest extends FlatSpec with Matchers {

    val x = Variable("x")
    val y = Variable("y")

    behavior of "Beta(x, y)"

    it should "differentiate with respect to x" in {
        val beta = Beta(x, y)
        println(beta.df(x))
    }

    "Beta(1, 1)" should "equal 1" in {
        Beta(One, One) shouldBe One
    }

}
