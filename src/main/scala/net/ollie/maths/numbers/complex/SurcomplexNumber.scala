//package net.ollie.maths.numbers.complex
//
//import net.ollie.maths.numbers.real.{EmptyForm, SurrealNumber}
//import net.ollie.maths.functions.angular.Angle
//import net.ollie.maths.numbers.PositiveRealNumber
//
///**
// * Created by Ollie on 06/01/14.
// */
//trait SurcomplexNumber
//        extends ComplexNumber {
//
//    override def re: SurrealNumber
//
//}
//
//class SurimaginaryNumber(override val coefficient: SurrealNumber)
//        extends ImaginaryNumber(coefficient)
//        with SurcomplexNumber {
//
//    override def re = EmptyForm
//
//    def abs: PositiveRealNumber = ???
//
//    def arg: Angle = ???
//
//}