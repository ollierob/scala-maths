package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.constants.One
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RootsOfUnityTest extends AnyFlatSpec with Matchers {

    behavior of "RootsOfUnity(2)"

    {

        val roots = new RootsOfUnity(2)

        it should "have principal" in {
            roots.principal shouldBe One
        }

        it should "have two values" in {
            val values = roots.values.toList
            values.size shouldBe 2
        }

    }

}
