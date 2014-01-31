package net.ollie.maths.numbers.complex

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.{Real, Zero}

/**
 * Created by Ollie on 31/01/14.
 */
class DualTest extends FlatSpec with Matchers {

    behavior of "2 + 3d"

    val d: Dual = Dual(2, 3)

    {

        it should "equal itself" in {
            d == d shouldBe true
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

    }

}
