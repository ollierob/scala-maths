package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 11/01/14.
 */
@RunWith(classOf[JUnitRunner])
class RampTest extends FlatSpec with Matchers {

    behavior of "Ramp(x)"

    {
        val x = new Variable("x")
        val r = Ramp(x)

        it should "differentiate" in {
            r.df(x)
        }

    }

}
