package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Integer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 26/01/14.
 */
@RunWith(classOf[JUnitRunner])
class MinTest extends FlatSpec with Matchers {

    "Min(3, 2, 1)" should "select 1" in {
        Min(Integer(3), Integer(2), Integer(1)) shouldBe Integer(1)
    }

}
