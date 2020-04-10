package net.ollie.maths.functions.special

import net.ollie.maths.Variable
import net.ollie.maths.numbers.constants.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 23/01/14.
 */
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
