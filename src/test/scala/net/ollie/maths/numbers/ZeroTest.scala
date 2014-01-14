package net.ollie.maths.numbers

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Ollie on 12/01/14.
 */
class ZeroTest extends FlatSpec with Matchers {

    behavior of "Zero"

    it should "invert" in {
        val i = Zero.inverse
        i.abs shouldBe (Infinity)
        i + 0 shouldBe (i)
    }

}
