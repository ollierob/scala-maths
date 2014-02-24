package net.ollie.maths.sequences

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.Integer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 24/02/14.
 */
@RunWith(classOf[JUnitRunner])
class PellSequenceTest extends FlatSpec with Matchers {

    "Pell(8)" should "equal 408" in {
        PellSequence(8) shouldBe Integer(408)
    }

}
