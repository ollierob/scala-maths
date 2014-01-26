package net.ollie.utils

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Ollie on 26/01/14.
 */
class CyclicTupleTest extends FlatSpec with Matchers {

    behavior of "CyclicTuple(1, 2, 3)"

    {

        val t: CyclicTuple3[Int, Int, Int] = (1, 2, 3)

        it should "have getters" in {
            t._1 shouldBe 1
            t._2 shouldBe 2
            t._3 shouldBe 3
        }

        it should "cycle left" in {
            t.cycleLeft._1 shouldBe 2
            t.cycleLeft._2 shouldBe 3
            t.cycleLeft._3 shouldBe 1
            t.cycleLeft shouldBe CyclicTuple(2, 3, 1)
        }

        it should "be contained in cycles" in {
            t.cycles contains t shouldBe true
            t.cycles contains t.cycleLeft shouldBe true
            t.cycles contains t.cycleLeft.cycleLeft shouldBe true
        }

        it should "not contain antisymmetric cycles" in {
            t.cycles contains CyclicTuple(1, 3, 2) shouldBe false
            t.cycles contains CyclicTuple(2, 1, 3) shouldBe false
            t.cycles contains CyclicTuple(3, 2, 1) shouldBe false
        }

    }

}
