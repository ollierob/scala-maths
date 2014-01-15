package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.real.Pi

/**
 * Created by Ollie on 15/01/14.
 */
class RealProductTest extends FlatSpec with Matchers {

    behavior of "2 * Pi"

    {

        val product = RealProduct(2, Pi)

        it should "multiply by 2" in {
            product * 2 shouldBe (RealProduct(4, Pi))
            2 * product shouldBe (RealProduct(4, Pi))
        }

    }

}
