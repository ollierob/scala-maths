//package net.ollie.maths.numbers.complex
//
//import net.ollie.maths.{NumberSeries, Number}
//import net.ollie.maths.numbers.{Real, Zero}
//
///**
// * A series or product of complex-like numbers of different types.
// * Created by Ollie on 05/02/14.
// */
//trait MixedComplex
//        extends Number {
//
//    type System = MixedComplex
//
//    def +(that: ComplexLike): MixedComplex
//
//    def ?+(that: Number): Option[Number] = that match {
//        case re: Real => Some(this + Complex(re))
//        case z: ComplexLike => Some(this + z)
//        case _ => None
//    }
//
//    def ?*(that: Number)(leftToRight: Boolean): Option[Number] = ???
//
//    def ?^(that: Number): Option[Number] = ???
//
//}
//
//object MixedComplex {
//
//    implicit def apply(re: Real): MixedComplex = Complex(re)
//
//    implicit def apply(z: ComplexLike): MixedComplex = new MixedComplexWrapper(z)
//
//    def add(x: MixedComplex, y: MixedComplex): MixedComplex = MixedComplex(Seq(x, y))
//
//    def apply(terms: Seq[MixedComplex]): MixedComplex = terms.filterNot(_.isEmpty) match {
//        case Nil => Zero
//        case term :: Nil => term
//        case otherwise => new MixedNumberSeries(otherwise)
//    }
//
//}
//
//class MixedComplexWrapper(val z: ComplexLike)
//        extends MixedComplex {
//
//    override def isEmpty = z.isEmpty
//
//    override def inverse = z.inverse
//
//    override def abs = z.abs
//
//    override def +(that: ComplexLike) = MixedComplex.add(z, that)
//
//    override def ?+(that: Number) = z ?+ that match {
//        case Some(m: ComplexLike) => Some(m)
//        case _ => super.?+(that)
//    }
//
//    override def toString = z.toString
//
//}
//
//class MixedNumberSeries(override val terms: Seq[MixedComplex])
//        extends NumberSeries(terms)
//        with MixedComplex {
//
//    protected[this] def tryAdd(left: ComplexLike, right: ComplexLike): Option[ComplexLike] = ???
//
//    def +(that: ComplexLike) = MixedComplex(simplify(MixedComplex(that), terms))
//
//    def inverse = ???
//
//    def abs = ???
//
//    protected[this] def tryAdd(left: MixedComplex, right: MixedComplex) = None
//
//}