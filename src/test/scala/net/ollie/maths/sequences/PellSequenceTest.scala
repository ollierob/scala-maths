package net.ollie.maths.sequences

import net.ollie.maths.numbers.Integer
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 24/02/14.
 */
class PellSequenceTest extends FlatSpec with Matchers {

    "Pell(8)" should "equal 408" in {
        PellSequence(8) shouldBe Integer(408)
    }

}
