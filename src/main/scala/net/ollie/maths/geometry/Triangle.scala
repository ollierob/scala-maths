//package net.ollie.maths.geometry
//
//import net.ollie.maths.functions.numeric.PositiveSquareRoot
//import net.ollie.maths.numbers.PositiveRealNumber
//
///**
// * Created by Ollie on 14/01/14.
// */
//sealed trait Triangle
//        extends Polygon {
//
//    final def numSides = 3
//
//    def perimeter = a + b + c
//
//    val s = semiperimeter
//
//    def area = PositiveSquareRoot(s * (s - a) * (s - b) * (s - c)).get
//
//    def sides: (PositiveRealNumber, PositiveRealNumber, PositiveRealNumber)
//
//    val a = sides._1
//
//    val b = sides._2
//
//    val c = sides._3
//
//    def inscribed: Circle
//
//    override final def equals(that: Any): Boolean = that match {
//        case triangle: Triangle => this equals triangle
//        case _ => super.equals(that)
//    }
//
//    def equals(that: Triangle): Boolean
//
//}
//
//object Triangle {
//
//    def equilateral(side: PositiveRealNumber): EquilateralTriangle = new EquilateralTriangle(side)
//
//    def sides(a: PositiveRealNumber, b: PositiveRealNumber, c: PositiveRealNumber): Triangle = (a, b, c) match {
//        case (_, _, _) if a == b && b == c => equilateral(a)
//        case _ => ???
//    }
//
//}
//
//class IsoscelesTriangle(val equal: PositiveRealNumber, val remaining: PositiveRealNumber)
//        extends Triangle {
//
//    def sides = (equal, equal, remaining)
//
//    def height: PositiveRealNumber = PositiveSquareRoot((equal ^ 2) - (remaining ^ 2) / 4).get
//
//    def inscribed: Circle = ???
//
//    def equals(that: Triangle) = that match {
//        case i: IsoscelesTriangle => this.equal == i.equal && this.remaining == i.remaining
//        case _ => false
//    }
//
//}
//
//class EquilateralTriangle(val side: PositiveRealNumber)
//        extends IsoscelesTriangle(side, side) {
//
//    override def area = side * PositiveSquareRoot(3) / 4
//
//    override def inscribed = Circle(side * PositiveSquareRoot(3) / 6)
//
//    override def height: PositiveRealNumber = side * PositiveSquareRoot(3) / 2
//
//    override def equals(triangle: Triangle) = triangle match {
//        case e: EquilateralTriangle => this.side == e.side
//        case _ => false
//    }
//
//}
