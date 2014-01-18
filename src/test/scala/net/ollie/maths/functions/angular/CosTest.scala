package net.ollie.maths.functions.angular

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Zero, One}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 12/01/14.
 */
@RunWith(classOf[JUnitRunner])
class CosTest extends FlatSpec with Matchers {

    "Cos(0)" should "equal 1" in {
        Cos(Zero) shouldBe (One)
    }

    {

        behavior of "Cos(x)"

        val x = Variable("x")
        val cos = Cos(x)

        it should "replace" in {
            cos.replace(x, Zero) shouldBe (One)
            //cos(Zero) shouldBe (One) //TODO
        }

    }

}
