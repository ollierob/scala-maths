package net.ollie.maths.functions.angular

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import net.ollie.maths.numbers.Zero
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 10/01/14.
 */
@RunWith(classOf[JUnitRunner])
class SphericalHarmonicTest extends FlatSpec with Matchers {

    behavior of "Y(0,0)"

    {
        val theta = new Variable("theta")
        val phi = new Variable("phi")
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

    }

}
