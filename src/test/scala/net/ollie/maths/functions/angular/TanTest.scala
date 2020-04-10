package net.ollie.maths.functions.angular

import net.ollie.maths.numbers.constants.Zero
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 18/01/14.
 */
class TanTest extends FlatSpec with Matchers {

    "Tan(0)" should "equal 0" in {
        Tan(0) shouldBe (Zero)
    }

}
