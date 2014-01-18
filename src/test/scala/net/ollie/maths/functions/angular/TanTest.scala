package net.ollie.maths.functions.angular

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Zero

/**
 * Created by Ollie on 18/01/14.
 */
class TanTest extends FlatSpec with Matchers {

    "Tan(0)" should "equal 0" in {
        Tan(0) shouldBe (Zero)
    }

}
