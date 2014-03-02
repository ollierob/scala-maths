package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.constants.{Zero, Two, One}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import net.ollie.maths.Variable

/**
 * Created by Ollie on 02/03/14.
 */
@RunWith(classOf[JUnitRunner])
class RisingFactorialTest extends FlatSpec with Matchers {

    val x = Variable("x")

    "RisingFactorial(x, 0)" should "be 1" in {
        RisingFactorial(0, 0) shouldBe One
        RisingFactorial(1, 0) shouldBe One
        RisingFactorial(2, 0) shouldBe One
    }

    "RisingFactorial(x, 1)" should "be x" in {
        RisingFactorial(Zero, One) shouldBe Zero
        RisingFactorial(One, One) shouldBe One
        RisingFactorial(Two, One) shouldBe Two
    }

}
