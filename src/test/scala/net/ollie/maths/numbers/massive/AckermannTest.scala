package net.ollie.maths.numbers.massive

import net.ollie.maths.numbers.Natural
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 05/08/2015.
 */
@RunWith(classOf[JUnitRunner])
class AckermannTest extends FlatSpec with Matchers {

    "A(0,n)" should "equal n+1" in {
        Ackermann(0, 0).closestReal shouldBe Natural(1)
        Ackermann(0, 1).closestReal shouldBe Natural(2)
        Ackermann(0, 2).closestReal shouldBe Natural(3)
        Ackermann(0, 3).closestReal shouldBe Natural(4)
    }

    "A(m,0)" should "equal A(m-1,1)" in {
        Ackermann(2, 0) shouldBe Ackermann(1, 1)
        Ackermann(3, 0) shouldBe Ackermann(2, 1)
        Ackermann(4, 0) shouldBe Ackermann(3, 1)
    }

    "A(1,1)" should "equal 3" in {
        Ackermann(1, 1).closestReal shouldBe Natural(3)
    }

    "A(1,2)" should "equal 4" in {
        Ackermann(1, 2).closestReal shouldBe Natural(4)
    }

    "A(2,1)" should "equal 5" in {
        Ackermann(2, 1).closestReal shouldBe Natural(5)
    }

}
