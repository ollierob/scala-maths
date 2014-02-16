package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Precision._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import net.ollie.maths.numbers.Real

/**
 * Created by Ollie on 15/02/14.
 */
@RunWith(classOf[JUnitRunner])
class RootsTest extends FlatSpec with Matchers {

    behavior of "Cube roots of 2"

    {

        val cubeRoots = Roots(2, 3)

        it should "have two complex and one real root" in {
            println(cubeRoots.values)
            println(cubeRoots.values.toSeq.map(_.isEmpty))
            println(cubeRoots.values.toSeq.map(Real(_)))
            cubeRoots.values.size shouldBe 3
            cubeRoots.values.filter(Real(_).isDefined).size shouldBe 1
        }

        it should "have principal" in {
            cubeRoots.principal.toReal.isDefined shouldBe true
            cubeRoots.principal.toReal.get.evaluate(4 dp) shouldBe BigDecimal("1.2599")
        }

    }

}
