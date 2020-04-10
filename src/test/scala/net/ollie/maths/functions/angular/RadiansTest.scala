package net.ollie.maths.functions.angular

import net.ollie.maths.functions.angular.Angle._
import net.ollie.maths.numbers.constants.Zero
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 12/01/14.
 */
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
