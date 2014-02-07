package net.ollie.utils

/**
 * Created by Ollie on 06/02/14.
 */
trait HomogeneousTuple[T]
        extends Product {

    override def productIterator: Iterator[T] = iterator

    def iterator: Iterator[T]

}

object Homogeneous3Tuple {

    implicit def apply[T](tuple: (T, T, T)) = new Homogeneous3Tuple(tuple._1, tuple._2, tuple._3)

}

class Homogeneous3Tuple[T](a: T, b: T, c: T)
        extends Tuple3(a, b, c)
        with HomogeneousTuple[T] {

    override def iterator = new Iterator[T] {

        var index = 0;

        override def hasNext: Boolean = index < 3

        override def next(): T = {
            index += 1
            index match {
                case 1 => a
                case 2 => b
                case 3 => c
                case _ => throw new NoSuchElementException
            }
        }

    }

}