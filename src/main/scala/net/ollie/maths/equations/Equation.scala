package net.ollie.maths.equations

import net.ollie.maths.{Variable, Expression}

/**
 * Created by Ollie on 09/03/14.
 */
trait Equation {

    def left: Expression

    def right: Expression

    def variables: Set[Variable] = left.variables ++ right.variables

}
