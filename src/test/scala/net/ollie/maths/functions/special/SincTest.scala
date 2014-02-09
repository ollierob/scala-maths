package net.ollie.maths.functions.special

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 23/01/14.
 */
@RunWith(classOf[JUnitRunner])
class SincTest extends FlatSpec with Matchers {

    "Sinc(0)" should "equal 0" in {
        Sinc(Zero) shouldBe One
    }

    "Sinc(x) at x=0" should "equal 0" in {
        val x = Variable("x")
        val sinc = Sinc(x)
        sinc.replace(x, 0).toConstant shouldBe Some(One)
    }

}
