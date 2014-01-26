package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Integer

/**
 * Created by Ollie on 26/01/14.
 */
class MinTest extends FlatSpec with Matchers {

    "Min(3, 2, 1)" should "select 1" in {
        Min(Integer(3), Integer(2), Integer(1)) shouldBe Integer(1)
    }

}
