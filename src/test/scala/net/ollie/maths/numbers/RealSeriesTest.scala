package net.ollie.maths.numbers

import net.ollie.maths.numbers.real.Pi
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 14/01/14.
 */
@RunWith(classOf[JUnitRunner])
class RealSeriesTest extends FlatSpec with Matchers {

    behavior of "1 + Pi"

    {

        val series = RealSeries(1, Pi)

        it should "add 0" in {
            series + 0 shouldBe (series)
        }

        it should "add 1" in {
            series + 1 shouldBe (RealSeries(2, Pi))
            1 + series shouldBe (RealSeries(2, Pi))
        }

        it should "add to itself" in {
            series + series shouldBe (RealSeries(2, 2 * Pi))
        }

    }

}
