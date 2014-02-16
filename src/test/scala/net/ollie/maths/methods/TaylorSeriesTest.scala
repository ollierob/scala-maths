package net.ollie.maths.methods

import net.ollie.maths.Variable
import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.functions.angular.Sin
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.Real
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * Created by Ollie on 12/01/14.
 */
@RunWith(classOf[JUnitRunner])
class TaylorSeriesTest extends FlatSpec with Matchers {

    behavior of "TaylorSeries(x)"

    it should "evaluate" in {
        val x = Variable("x")
        val series: Real = TaylorSeries(Sin, Zero, Zero)
        series shouldBe Zero
    }

}
