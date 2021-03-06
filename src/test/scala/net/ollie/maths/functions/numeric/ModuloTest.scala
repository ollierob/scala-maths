package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.Integer
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 16/02/14.
 */
class ModuloTest extends FlatSpec with Matchers {

    behavior of "5 mod 2"

    it should "have quotient 2" in {
        Modulo(5, 2).quotient shouldBe Integer(2)
    }

    it should "have remainder 1" in {
        Modulo(5, 2).remainder shouldBe Integer(1)
    }

    //    behavior of "10 % 3"
    //
    //    it should "have remainder 1" in {
    //        Modulo(10, 3).remainder shouldBe One
    //    }
    //
    //    it should "have quotient 2" in {
    //        Modulo(10, 3).quotient shouldBe Integer(2)
    //    }
    //
    //    behavior of "10 % 11"
    //
    //    it should "have quotient 0" in {
    //        Modulo(10, 11).quotient shouldBe Zero
    //    }

}
