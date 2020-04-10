package net.ollie.maths.functions.angular

import net.ollie.maths.numbers.Integer
import net.ollie.maths.numbers.Precision._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 26/01/14.
 */
class Wigner3jTest extends FlatSpec with Matchers {

    "Wigner3j((1, 0), (2, 0), (3, 0)" should "evaluate" in {
        val r = Wigner3j((1, 2, 3), (0, 0, 0))
        r.isEmpty shouldBe false
        r.evaluate(4 dp) shouldBe BigDecimal("-0.2928")
    }

    "Wigner3j((1, 0), (1, 0), (2, 0)" should "evaluate" in {
        val r = Wigner3j((1, 1, 2), (0, 0, 0))
        r.isEmpty shouldBe false
        r.evaluate(4 dp) shouldBe BigDecimal("0.3651")
    }

    "Wigner3j((1,0), (1,0), (1,0)" should "be empty" in {
        val r = Wigner3j((1, 1, 1), (0, 0, 0))
        r.isEmpty shouldBe true
    }

    "Wigner3j((1,1), (1,-1), (2,0)" should "evaluate" in {

        val r = Wigner3j((1, 1, 2), (1, -1, 0))

        r.l1 shouldBe Integer(1)
        r.l2 shouldBe Integer(1)
        r.l3 shouldBe Integer(2)

        r.m1 shouldBe Integer(1)
        r.m2 shouldBe Integer(-1)
        r.m3 shouldBe Integer(0)

        r.isEmpty shouldBe false
        r.evaluate(4 dp) shouldBe BigDecimal("0.1826")

    }

    "Wigner3j((1,1), (2,1), (3,-2))" should "evaluate" in {
        val r = Wigner3j((1, 2, 3), (1, 1, -2))
        r.isEmpty shouldBe false
        r.evaluate(4 dp) shouldBe BigDecimal("-0.3086")
    }

}
