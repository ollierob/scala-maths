package net.ollie.maths

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.{One, Pi}

/**
 * Created by Ollie on 11/01/14.
 */
@RunWith(classOf[JUnitRunner])
class VariableTest extends FlatSpec with Matchers {

    behavior of "x"

    {

        val x = Variable("x")

        it should "add to y" in {
            val y = Variable("y")
            val z = x + y
            z.isEmpty shouldBe (false)
        }

        it should "multiply by y" in {
            val y = Variable("y")
            val z = x * y
            z.isEmpty shouldBe (false)
        }

        it should "replace" in {
            x.replace(x, Pi) shouldBe Pi
            x.replace(Variable("y"), Pi) shouldBe (x)
        }

        it should "negate" in {
            (-x).replace(x, Pi) shouldBe -Pi
        }

        it should "integrate with respect to itself" in {
            val integral = x.integrate(x)
            integral.toConstant shouldBe None //Because of constant of integration
            integral.df(x) shouldBe x
        }

    }

}
