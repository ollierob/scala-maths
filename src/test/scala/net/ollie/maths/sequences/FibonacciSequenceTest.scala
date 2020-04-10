package net.ollie.maths.sequences

import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 19/02/14.
 */
class FibonacciSequenceTest extends FlatSpec with Matchers {

    "Fib(0)" should "be 0" in {
        FibonacciSequence(Zero) shouldBe Zero
    }

    "Fib(1)" should "be 1" in {
        FibonacciSequence(One) shouldBe One
    }

    val two: Natural = 2

    "Fib(2)" should "be 1" in {
        FibonacciSequence(two) shouldBe One
    }

    val three: Natural = 3

    "Fib(3)" should "be 2" in {
        FibonacciSequence(three) shouldBe two
    }

    //    "Fib(1111)" should "evaluate" in {
    //        val expected = BigInt("6851462981265369536304298877223231154064355390623195419885661484162849735541256952762360871448156142552148460793441585691068131682370855135019896825808086317430648360941203391832868742715640036246053259136014253626356840914521594989")
    //        FibonacciSequence(1111).evaluate shouldBe expected
    //    }

}
