package net.ollie.maths.geometry

import net.ollie.maths.numbers.{Integer, IntegerFraction}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 09/02/14.
 */
class SquareTest extends FlatSpec with Matchers {

    behavior of "Square(5)"

    val square = new Square(5)

    it should "have area = 25" in {
        square.area shouldBe Integer(25)
    }

    it should "have perimeter = 20" in {
        square.perimeter shouldBe Integer(20)
    }

    it should "have apothem = 5/2" in {
        square.apothem shouldBe IntegerFraction(5, 2)
    }

}
