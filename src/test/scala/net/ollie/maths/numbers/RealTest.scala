package net.ollie.maths.numbers

import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RealTest extends FlatSpec with Matchers {

    behavior of "1.2"

    it should "evaluate" in {
        val re = Real(1.2d)
    }

}