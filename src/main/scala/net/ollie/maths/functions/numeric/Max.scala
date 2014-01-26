package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers.Real

/**
 * Created by Ollie on 26/01/14.
 * @see http://mathworld.wolfram.com/Maximum.html
 */
object Max {

    def apply[N <: Ordered[M], M >: N](values: N*)(implicit cmp: Ordering[M]): N = values.max(cmp)

    def apply[N <: Real](values: N*): N = values.max(Real.RealArithmetic)

    //    def apply(terms: Seq[Expression]) = new Max(terms)

}

//class Max(val terms: Seq[Expression])
//        extends Aggregate {
//
//    override def toString = s"Max($terms)"
//
//    def toConstant: Option[Number] = {
//        var max: Option[Number with Ordered] = None
//        for (option <- terms.map(_.toConstant)) {
//            option match {
//                case Some(o: Number with Ordered) => max = Some(max match {
//                    case Some(m) => if (m > o) m else o //TODO how do we get this to compile?
//                    case _ => o
//                })
//                case _ => return None
//            }
//        }
//        max
//    }
//
//    private def maxOf[T <: Number with Ordered[M], M >: T](max: T, test: T): T = if (max > test) max else test
//
//    def isEmpty = terms.forall(_.isEmpty)
//
//    override def df(x: Variable) = Max(terms.map(_.df(x)))
//
//    protected[this] def apply(expressions: Seq[Expression]) = Max(expressions)
//
//}