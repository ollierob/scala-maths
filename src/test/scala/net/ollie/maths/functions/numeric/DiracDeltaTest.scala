package net.ollie.maths.functions.numeric

import net.ollie.maths.Variable
import net.ollie.maths.numbers.Infinite
import net.ollie.maths.numbers.constants.{Pi, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 10/01/14.
 */
class DiracDeltaTest extends FlatSpec with Matchers {

    "delta(0)" should "be infinite" in {
        DiracDelta(0).isInstanceOf[Infinite] shouldBe (true)
    }

    "delta(1)" should "be 0" in {
        DiracDelta(1) shouldBe (Zero)
    }

    "delta(x)" should "be 0 except at 0" in {
        val x = Variable("x")
        val d = DiracDelta(x)
        d.isEmpty shouldBe true
        d.replace(x, 1).isEmpty shouldBe (true)
        d.replace(x, 0).isInstanceOf[Infinite] shouldBe (true)
        d.replace(x, Pi).isEmpty shouldBe (true)
    }

}
