package net.ollie.maths.functions.angular

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 10/01/14.
 */
@RunWith(classOf[JUnitRunner])
class SphericalHarmonicTest extends FlatSpec with Matchers {

    val theta = new Variable("theta")
    val phi = new Variable("phi")

    behavior of "Y(0,0)"

    {

        val ylm = SphericalHarmonic(0, 0, theta, phi)

        it should "have zero order and degree" in {
            ylm.l shouldBe (Zero)
            ylm.order shouldBe (Zero)
            ylm.m shouldBe (Zero)
            ylm.degree shouldBe (Zero)
        }

        it should "differentiate" in {
            ylm.df(theta).isEmpty shouldBe (true)
            ylm.df(phi).isEmpty shouldBe (true)
        }

        it should "conjugate" in {
            ylm.conjugate shouldBe ylm
        }

    }

    behavior of "Y(1, 1)"

    {

        val ylm = SphericalHarmonic(1, 1, theta, phi)

        it should "conjugate" in {
            val c = ylm.conjugate
            c.l shouldBe ylm.l
            c.m shouldBe -(ylm.m)
            c.conjugate shouldBe ylm
        }

        it should "differentiate" in {
            val df = ylm.df(theta)
            println(df)
        }

    }

}
