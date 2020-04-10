package net.ollie.maths.numbers

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.One
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Ollie on 14/01/14.
 */
class RealExponentTest extends AnyFlatSpec with Matchers {

    behavior of "1^3"

    it should "equal 1" in {
        val p = RealExponent(1, 3)
        p shouldEqual One
    }

    it should "equal 1^4" in {
        val p1 = RealExponent(1, 3)
        val p2 = RealExponent(1, 4)
        p1 shouldEqual p2
        p2 shouldEqual p1
    }

    behavior of "5 ^ (2/3)"

    {

        val power: RealExponent = Integer(5) ^ IntegerFraction(2, 3)

        it should "have 3 values" in {
            power.values.size shouldBe 3
        }

        it should "have real principal" in {
            power.principal.toReal.isDefined shouldBe true
            val principal: Real = power.principal.toReal.get
            principal.evaluate(4 dp) shouldBe BigDecimal("2.9240")
        }

        it should "have principal in values" in {
            power.values.contains(power.principal) shouldBe true
        }

        it should "have complex conjugate non-principals" in {
            val complexValues: Seq[Complex] = (new ArrayBuffer[Complex](3) ++ power.values -= power.principal).toList
            val z1 = complexValues(0);
            val z2 = complexValues(1);
            z1.conjugate shouldBe z2;
            z1.re.evaluate(4 dp) shouldBe BigDecimal("-1.4620")
            z1.im.abs.evaluate(4 dp) shouldBe BigDecimal("2.5323")
        }

    }

}
