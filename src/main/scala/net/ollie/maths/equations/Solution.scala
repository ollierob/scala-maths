package net.ollie.maths.equations

import net.ollie.maths.{Constant, Variable}

/**
 * Created by Ollie on 09/03/14.
 */
sealed trait Solution

trait NoSolution
        extends Solution

trait UniqueSolution
        extends Solution {

    def solution: Map[Variable, Constant]

}

trait FinitelyManySolutions
        extends Solution {

    def solutions: Set[Map[Variable, Constant]]

}

trait InfinitelyManySolutions
        extends Solution
