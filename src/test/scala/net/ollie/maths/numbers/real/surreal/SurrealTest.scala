package net.ollie.maths.numbers.real.surreal

import net.ollie.maths.numbers.{Integer, Zero}
import org.scalatest.{FlatSpec, Matchers}
import Surreal._

/**
 * Created by Ollie on 28/01/14.
 */
class SurrealTest extends FlatSpec with Matchers {

    behavior of "Empty form"

    val nothing: Surreal = SurrealSet() | SurrealSet()

    it should "be empty" in {
        nothing.isEmpty shouldBe true
    }

    behavior of "1"

    val one: Surreal = Zero | SurrealSet()

    it should "not be empty" in {
        println(one)
        one.isEmpty shouldBe false
    }

    it should "be nearest to 1" in {
        one.nearest shouldBe Integer(1)
    }

    behavior of "2"

    val two: Surreal = one | SurrealSet()

    it should "not be empty" in {
        println(two)
        two.isEmpty shouldBe false
    }

    it should "be nearest to 2" in {
        two.nearest shouldBe Integer(2)
    }

    "{1|3}" should "be 2" in {
        Integer(1) | Integer(3) shouldBe Integer(2)
    }

}
