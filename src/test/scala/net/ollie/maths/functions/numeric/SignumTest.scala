package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.constants.One
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 22/02/14.
 */
@RunWith(classOf[JUnitRunner])
class SignumTest extends FlatSpec with Matchers {

    "Signum(+n)" should "be 1" in {
        Signum(1) shouldBe One
        Signum(2) shouldBe One
        Signum(3) shouldBe One
        Signum(4) shouldBe One
    }

}
