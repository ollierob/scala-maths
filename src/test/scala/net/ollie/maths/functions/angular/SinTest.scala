package net.ollie.maths.functions.angular

import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.Variable
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.{Zero, Precision}
import Precision._

/**
 * Created by Ollie on 03/01/14.
 */
@RunWith(classOf[JUnitRunner])
class SinTest extends FlatSpec with Matchers {

    val x = Variable("x")
    val sin = Sin(x)

    //    {
    //        behavior of "sin(x)"
    //
    //        val x = Variable("x")
    //        val sin = Sin(x)
    //
    //        it should "not be empty" in {
    //            sin.isEmpty shouldBe (false)
    //        }
    //
    //        it should "replace" in {
    //            sin.replace(x, 5).toConstant shouldBe (Some(Sin(5)))
    //        }
    //
    //        //        it should "negate" in {
    //        //            (-sin).replace(x, 5).toConstant shouldBe (Some(-Sin(5)))
    //        //        }
    //
    //    }

    behavior of "sin(0)"

    it should "replace" in {
        sin.replace(x, 0).toConstant.get shouldBe (Zero)
    }

    "sin(1)" should "evaluate" in {
        Sin(1).evaluate(4 dp).toString shouldBe ("0.8415")
    }

    //    "sin(2)" should "evaluate" in {
    //        Sin(2).evaluate(4 dp).toString shouldBe ("0.9093")
    //    }

}
