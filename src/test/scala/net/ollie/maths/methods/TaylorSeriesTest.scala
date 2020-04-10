package net.ollie.maths.methods

import net.ollie.maths.Variable
import net.ollie.maths.functions.angular.Sin
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Zero
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 12/01/14.
 */
class TaylorSeriesTest extends FlatSpec with Matchers {

    behavior of "TaylorSeries(x)"

    it should "evaluate" in {
        val x = Variable("x")
        val series: Real = TaylorSeries(Sin, Zero, Zero)
        series shouldBe Zero
    }

}
