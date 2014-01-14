package net.ollie.maths.methods

import net.ollie.maths.Variable
import net.ollie.maths.numbers.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.functions.angular.Sin

/**
 * Created by Ollie on 12/01/14.
 */
class TaylorSeriesTest extends FlatSpec with Matchers {

    behavior of "TaylorSeries(x)"

    it should "evaluate" in {
        val x = Variable("x")
        val series = TaylorSeries(Sin, Zero, Zero)
        series.replace(x, One) shouldBe (One)
    }

}
