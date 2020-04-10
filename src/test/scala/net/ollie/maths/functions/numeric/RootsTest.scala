package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.Precision._
import net.ollie.maths.numbers.{Integer, Real}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 15/02/14.
 */
class RootsTest extends FlatSpec with Matchers {

    behavior of "Cube roots of 2"

    {

        val cubeRoots = Roots(2, 3)

        it should "have two complex and one real root" in {
            cubeRoots.values.size shouldBe 3
            cubeRoots.values.filter(Real(_).isDefined).size shouldBe 1
            cubeRoots.values.map(_.isEmpty) shouldBe Set(false)
        }

        it should "have principal" in {
            cubeRoots.principal.toReal.isDefined shouldBe true
            cubeRoots.principal.toReal.get.evaluate(4 dp) shouldBe BigDecimal("1.2599")
        }

    }

    behavior of "Cube roots of 8"

    {

        val cubeRoots = Roots(8, 3)

        it should "have two complex and one real root" in {
            cubeRoots.values.size shouldBe 3
            cubeRoots.values.filter(Real(_).isDefined).size shouldBe 1
        }

        it should "contain 2" in {
            cubeRoots.values.contains(Integer(2)) shouldBe true
        }

        it should "contain -1 + i Sqrt(2)" in {
            val root = cubeRoots.values.filterNot(Real(_).isDefined).iterator.next
            root.re shouldBe Integer(-1)
            root.im.abs shouldBe PositiveSquareRoot(3)
        }

    }

}
