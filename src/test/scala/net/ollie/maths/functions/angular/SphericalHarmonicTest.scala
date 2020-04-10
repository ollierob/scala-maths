package net.ollie.maths.functions.angular

import net.ollie.maths.Variable
import net.ollie.maths.numbers.constants.Zero
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 10/01/14.
 */
class SphericalHarmonicTest extends FlatSpec with Matchers {

    val theta = Variable("theta")
    val phi = Variable("phi")

    behavior of "Y(0,0)"

    {

        val ylm = SphericalHarmonic(0, 0, theta, phi)

        it should "have zero order and degree" in {
            ylm.degree shouldBe Zero
            ylm.order shouldBe Zero
            ylm.order shouldBe Zero
            ylm.degree shouldBe Zero
        }

        it should "differentiate" in {
            ylm.df(theta).isEmpty shouldBe true
            ylm.df(phi).isEmpty shouldBe true
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
            c.degree shouldBe ylm.degree
            c.order shouldBe -(ylm.order)
            c.conjugate shouldBe ylm
        }

        it should "differentiate" in {
            val df = ylm.df(theta)
        }

    }

}
