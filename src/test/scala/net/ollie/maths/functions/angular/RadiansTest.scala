package net.ollie.maths.functions.angular

import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.numbers.Zero
import Angle._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * Created by Ollie on 12/01/14.
 */
@RunWith(classOf[JUnitRunner])
class RadiansTest extends FlatSpec with Matchers {


    behavior of "0 radians"

    {

        val zero = Zero radians

        it should "negate" in {
            val minusZero: Radians = -zero
            minusZero shouldBe (zero)
        }

    }

}
