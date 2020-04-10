package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.Integer
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 22/02/14.
 */
class RoundTest extends FlatSpec with Matchers {

    "Floor(3 / 2)" should "be 1" in {
        Floor(Integer(3) / Integer(2)) shouldBe Integer(1)
    }

    "Ceil(3 / 2)" should "be 2" in {
        Ceiling(Integer(3) / Integer(2)) shouldBe Integer(2)
    }

}
