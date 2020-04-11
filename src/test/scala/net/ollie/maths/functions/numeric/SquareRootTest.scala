package net.ollie.maths.functions.numeric

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.Real

class SquareRootTest extends AnyFlatSpec with Matchers {

    behavior of "Sqrt(16)"

    {

        it should "values of positive real" in {
            val sqrt = SquareRoot(16)
            val values = sqrt.values.toList
            values.size shouldBe 2
            values.apply(0).evaluate(8 dp) shouldBe 4
            values.apply(1).evaluate(8 dp) shouldBe -4
        }

        it should "values of real" in {
            val re: Real = 16
            val sqrt = SquareRoot(re)
            val values = sqrt.values.toList
            values.size shouldBe 2

            val z0 = values.apply(0)
            z0.isReal shouldBe true
            z0.re.evaluate(8 dp) shouldBe 4

            val z1 = values.apply(1)
            z1.isReal shouldBe true
            z1.re.evaluate(8 dp) shouldBe -4

        }

    }

}
