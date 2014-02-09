package net.ollie.maths.functions.angular

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.constants.Zero
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 18/01/14.
 */
@RunWith(classOf[JUnitRunner])
class TanTest extends FlatSpec with Matchers {

    "Tan(0)" should "equal 0" in {
        Tan(0) shouldBe (Zero)
    }

}
