package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.{Zero, One, Pi}

/**
 * Created by Ollie on 15/01/14.
 */
@RunWith(classOf[JUnitRunner])
class RealProductTest extends FlatSpec with Matchers {

    behavior of "2 * Pi"

    {

        val product = RealProduct(2, Pi)

        it should "multiply by zero" in {
            product * Zero shouldBe (Zero)
            Zero * product shouldBe (Zero)
        }

        it should "multiply by 1" in {
            product * One shouldBe (product)
        }

        it should "multiply by 2" in {
            product * 2 shouldBe (RealProduct(4, Pi))
            2 * product shouldBe (RealProduct(4, Pi))
        }

        it should "multiply by itself" in {
            product * product shouldBe (RealProduct(Seq(4, Pi, Pi)))
        }

    }

}
