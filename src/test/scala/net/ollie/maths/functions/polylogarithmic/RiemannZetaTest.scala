//package net.ollie.maths.functions.polylogarithmic
//
//import org.scalatest.{Matchers, FlatSpec}
//import net.ollie.maths.numbers.constants.Half
//import org.scalatest.junit.JUnitRunner
//import org.junit.runner.RunWith
//import net.ollie.maths.numbers.Precision._
//
///**
// * Created by Ollie on 19/02/14.
// */
//@RunWith(classOf[JUnitRunner])
//class RiemannZetaTest extends FlatSpec with Matchers {
//
//    "Zeta(0)" should "be -1/2" in {
//        RiemannZeta(0) shouldBe -Half
//    }
//
//    "Zeta(2)" should "be Pi^2/6" in {
//        RiemannZeta(2).evaluate(4 dp) shouldBe BigDecimal("1.6450")
//    }
//
//}
