package net.ollie.maths.numbers.surreal

import net.ollie.maths.numbers.Integer
import org.scalatest.{FlatSpec, Matchers}
import Surreal._
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 28/01/14.
 */
class SurrealTest extends FlatSpec with Matchers {

    behavior of "Empty form"
    val nothing: Surreal = SurrealSet() | SurrealSet()

    {

        it should "be empty" in {
            nothing.isEmpty shouldBe true
        }

        it should "be expressible in other ways" in {
            nothing shouldBe Surreal()
        }

    }

    behavior of "1"
    val one: Surreal = Zero | SurrealSet()

    {

        it should "not be empty" in {
            one.isEmpty shouldBe false
        }

        it should "be nearest to 1" in {
            one.nearest shouldBe Integer(1)
        }

    }

    behavior of "2"
    val two: Surreal = one | SurrealSet()

    {

        it should "not be empty" in {
            two.isEmpty shouldBe false
        }

        it should "be nearest to 2" in {
            two.nearest shouldBe Integer(2)
        }

    }

    "{1|3}" should "be 2" in {
        Integer(1) | Integer(3) shouldBe Integer(2)
    }

}
