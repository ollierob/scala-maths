package net.ollie.maths.geometry

import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.One
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 06/02/14.
 */
class TriangleTest extends FlatSpec with Matchers {

    behavior of "Triangle(3, 4, 5)"

    {

        val t: Triangle = Triangle(3, 4, 5)

        it should "have inradius = 1" in {
            t.inradius shouldBe One
        }

        it should "have area = 6" in {
            t.area shouldBe Real(6)
        }

        it should "have a right angle" in {
            t.isRight shouldBe true
        }

    }

}
