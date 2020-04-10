package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.Integer
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 26/01/14.
 */
class MinTest extends FlatSpec with Matchers {

    "Min(3, 2, 1)" should "select 1" in {
        Min(Integer(3), Integer(2), Integer(1)) shouldBe Integer(1)
    }

}
