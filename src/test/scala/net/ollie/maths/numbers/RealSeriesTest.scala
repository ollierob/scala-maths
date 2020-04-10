package net.ollie.maths.numbers

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.constants.Pi
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 14/01/14.
 */
class RealSeriesTest extends FlatSpec with Matchers {

    behavior of "1 + Pi"

    {

        val series = RealSeries(1, Pi)

        it should "add 0" in {
            series + 0 shouldBe (series)
        }

        it should "add 1" in {
            series + 1 shouldBe RealSeries(2, Pi)
            1 + series shouldBe RealSeries(2, Pi)
        }

        it should "add to itself" in {
            val s2 = series + series
            s2.evaluate(4 dp) shouldBe BigDecimal("8.2832")
            s2 shouldBe 2 + (2 * Pi)
        }

    }

}
