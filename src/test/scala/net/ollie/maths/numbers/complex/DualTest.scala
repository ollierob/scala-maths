package net.ollie.maths.numbers.complex

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.{Real, Zero}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 31/01/14.
 */
@RunWith(classOf[JUnitRunner])
class DualTest extends FlatSpec with Matchers {

    behavior of "2 + 3d"

    val r: Real = 2
    val u: Real = 3
    val d: Dual = Dual(r, u)

    {

        it should "equal itself" in {
            d == d shouldBe true
        }

        it should "invert" in {
            d.inverse shouldBe Dual(r.inverse, -u / (r.squared))
        }


        it should "add to a real" in {
            d + 5 shouldBe Dual(7, 3)
            5 + d shouldBe Dual(7, 3)
        }

        it should "multiply by a real" in {
            d * 2 shouldBe Dual(4, 6)
            2 * d shouldBe Dual(4, 6)
        }

        it should "divide by a real" in {
            d / 2 shouldBe Dual(Real(1), Real(3) / Real(2))
        }

        it should "add to itself" in {
            (d + d) shouldBe Dual(4, 6)
        }

        it should "subtract itself" in {
            (d - d) shouldBe DualZero
            (d - d) shouldBe Zero
        }

        it should "multiply by itself" in {
            (d * d) shouldBe Dual(4, 12)
        }

        it should "divide by itself" in {
            d / d shouldBe Dual(1, 0)
        }

    }

}
