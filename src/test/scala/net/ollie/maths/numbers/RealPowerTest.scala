package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class RealPowerTest extends FlatSpec with Matchers {

    behavior of "1^3"

    it should "equal 1" in {
        val p = RealPower(1, 3)
        p shouldEqual (One)
    }

    it should "equal 1^4" in {
        val p1 = RealPower(1, 3)
        val p2 = RealPower(1, 4)
        p1 shouldEqual (p2)
        p2 shouldEqual (p1)
    }

}
