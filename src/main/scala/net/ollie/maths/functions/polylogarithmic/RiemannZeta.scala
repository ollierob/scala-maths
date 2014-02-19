//package net.ollie.maths.functions.polylogarithmic
//
//import net.ollie.maths.Expression
//import net.ollie.maths.functions.{RealFunctionBuilder, BuiltFunction}
//import net.ollie.maths.numbers.{Natural, Precision, Real, Integer}
//import net.ollie.maths.methods.Series
//import net.ollie.maths.numbers.constants.{Zero, Half, One}
//import net.ollie.maths.functions.numeric.Ln
//
///**
// * Created by Ollie on 17/02/14.
// * @see http://mathworld.wolfram.com/RiemannZetaFunction.html
// */
//object RiemannZeta
//        extends RealFunctionBuilder {
//
//    def apply(re: Real): Real = re match {
//        case Zero => empty
//        case i: Integer => apply(i)
//        case _ => ???
//    }
//
//    def apply(i: Integer): Real = {
//        if (i.isEmpty) empty
//        else new IntegerZeta(i)
//    }
//
//    protected[this] def empty = -Half
//
//    protected[this] def create(expr: Expression) = new RiemannZetaOf(expr)
//
//}
//
//trait RiemannZeta {
//
//    def of: Expression
//
//    override def toString = s"RiemannZeta($of)"
//
//}
//
//class RiemannZetaOf(val of: Expression)
//        extends BuiltFunction
//        with RiemannZeta {
//
//    def isEmpty = false
//
//    protected[this] def derivative(x: Expression) = -Series(dk _, 2)
//
//    private def dk(k: Integer): Expression = Ln(k) / (k ^ of)
//
//    override protected[this] def builder = RiemannZeta
//
//}
//
//class IntegerZeta(val of: Integer)
//        extends Real
//        with RiemannZeta {
//
//    require(!of.isEmpty)
//
//    def isEmpty = false
//
//    private lazy val series = Series(nth _, One)
//
//    private def nth(k: Natural): Real = 1 / (k ^ of)
//
//    protected[this] def doEvaluate(precision: Precision) = series.evaluate(precision)
//
//}