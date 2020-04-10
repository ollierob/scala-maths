package net.ollie.maths.functions.numeric

import net.ollie.maths.Variable
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 11/01/14.
 */
class RampTest extends FlatSpec with Matchers {

    behavior of "Ramp(x)"

    {
        val x = Variable("x")
        val r = Ramp(x)

        it should "differentiate" in {
            r.df(x)
        }

    }

}
