package net.ollie.maths.functions.numeric

import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Infinity, IntegerNumber, One, Zero}
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 18/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ExpTest extends FlatSpec with Matchers {

    val x = Variable("x")

    "Exp(0)" should "be 1" in {
        Exp(Zero) shouldBe (One)
        Exp(Zero).isEmpty shouldBe false
    }

    "Exp(1)" should "not be empty" in {
        Exp(1).isEmpty shouldBe false
    }

    "Exp(-1000)" should "not be empty" in {
        Exp(-1000).isEmpty shouldBe false
    }

    "Exp(-∞)" should "be 0" in {
        Exp(-Infinity) shouldBe Zero
    }

    "Exp(x):x->0" should "be 1" in {
        val exp = Exp(x)
        exp.replace(x, 0).toConstant.get shouldBe (One)
    }

    "Exp(2x)" should "differentiate to 2 Exp(2x)" in {
        val two = IntegerNumber(2)
        val exp = Exp(two * x)
        val d = exp.df(x)
        println(d)
        d.replace(x, 0).toConstant.get shouldBe (two)
    }

    "Exp(Ln(x))" should " be x" in {
        Exp(Ln(x)) shouldBe (x)
    }

    "Exp(y):y->Ln(x)" should "be x" in {
        val y = Variable("y")
        val exp = Exp(y)
        val replaced = exp.replace(y, Ln(x))
        replaced shouldBe (x)
    }

}
