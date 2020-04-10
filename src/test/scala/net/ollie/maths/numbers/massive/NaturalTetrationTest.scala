package net.ollie.maths.numbers.massive

import net.ollie.maths.numbers.Natural
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 04/08/2015.
 */
class NaturalTetrationTest extends FlatSpec with Matchers {

    val one = Natural(1)
    val two = Natural(2)

    "1 ^^ n" should "evalute" in {
        NaturalTetration(1, 1).closestReal shouldBe one
        NaturalTetration(1, 2).closestReal shouldBe one
        NaturalTetration(1, 3).closestReal shouldBe one
    }

    "2 ^^ n" should "evalute" in {
        NaturalTetration(2, 1).closestReal shouldBe Natural(2)
        NaturalTetration(2, 2).closestReal shouldBe Natural(4)
        NaturalTetration(2, 3).closestReal shouldBe Natural(16)
        NaturalTetration(2, 4).closestReal shouldBe Natural(65536)
    }

    "3 ^^ n" should "evalute" in {
        NaturalTetration(3, 1).closestReal shouldBe Natural(3)
        NaturalTetration(3, 2).closestReal shouldBe Natural(27)
        NaturalTetration(3, 3).closestReal shouldBe Natural(7625597484987l)
    }

}
