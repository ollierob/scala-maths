package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.constants.One
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 22/02/14.
 */
class SignumTest extends FlatSpec with Matchers {

    "Signum(+n)" should "be 1" in {
        Signum(1) shouldBe One
        Signum(2) shouldBe One
        Signum(3) shouldBe One
        Signum(4) shouldBe One
    }

}
