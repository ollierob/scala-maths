package net.ollie.maths.functions.numeric

import net.ollie.maths.Variable
import net.ollie.maths.numbers.{Precision, Infinity, Integer}
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.{Zero, One}
import Precision._

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
        Exp(1).evaluate(4 dp) shouldBe BigDecimal("2.7183")
    }

    "Exp(2)" should "evaluate" in {
        Exp(2).evaluate(4 dp) shouldBe BigDecimal("7.3891")
    }

    "Exp(3)" should "evaluate" in {
        Exp(3).evaluate(4 dp) shouldBe BigDecimal("20.0855")
    }

    "Exp(-1000)" should "not be empty" in {
        Exp(-1000).isEmpty shouldBe false
    }

    "Exp(-∞)" should "be 0" in {
        Exp(-Infinity) shouldBe Zero
    }

    "Exp(x):x->0" should "be 1" in {
        val exp = Exp(x)
        exp.replace(x, 0).toConstant.get shouldBe One
    }

    "Exp(2x)" should "differentiate to 2 Exp(2x)" in {
        val two = Integer(2)
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
