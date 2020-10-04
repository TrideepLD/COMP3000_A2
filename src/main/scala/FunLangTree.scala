/**
 * FunLang source program tree definition.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

/**
 * Module containing tree structures for representing FunLang programs.
 */
object FunLangTree {

    /**
     * The common supertype of all source tree nodes.
     */
    sealed abstract class FunLangNode

    /**
     * A FunLang program is an expression.
     */
    case class Program (exp : Exp) extends FunLangNode

    /**
     * Definition groups.
     */
    sealed abstract class DefnGroup extends FunLangNode

    /**
     * Multiple mutually recursive function definitions.
     */
    case class FunGroup (funs : Vector[Fun]) extends DefnGroup

    /**
     * Single function definition.
     */
    case class Fun (idndef : IdnDef, arg : Arg, body : Exp) extends FunLangNode

    /**
     * Function argument.
     */
    case class Arg (idndef : IdnDef, tipe : Type) extends FunLangNode

    /**
     * Single value definition.
     */
    case class Val (idndef : IdnDef, exp : Exp) extends DefnGroup

    /**
     * Base class of all types.
     */
    sealed abstract class Type

    /**
     * Boolean type
     */
    case class BoolType () extends Type

    /**
     * Function type
     */
    case class FunType (from : Type, to : Type) extends Type

    /**
     * Integer type
     */
    case class IntType () extends Type

    /**
     * Common superclass of expressions.
     */
    sealed abstract class Exp extends FunLangNode

    /**
     * Application expression.
     */
    case class AppExp (fn : Exp, arg : Exp) extends Exp

    /**
     * Block expression.
     */
    case class BlockExp (defns : Vector[DefnGroup], exp : Exp) extends Exp

    /**
     * Boolean-valued expression.
     */
    case class BoolExp (b : Boolean) extends Exp

    /**
     * Equality expression compares the left and right expressions for equality.
     */
    case class EqualExp (left : Exp, right : Exp) extends Exp

    /**
     * Conditional expression.
     */
    case class IfExp (cond : Exp, thenExp : Exp, elseExp : Exp) extends Exp

    /**
     * Less than expression compares the left and right numeric expressions for less-than order.
     */
    case class LessExp (left : Exp, right : Exp) extends Exp

    /**
     * An expression whose value is the difference between the values of
     * two expressions.
     */
    case class MinusExp (left : Exp, right : Exp) extends Exp

    /**
     * Integer-valued numeric expression.
     */
    case class IntExp (n : Int) extends Exp

    /**
     * An expression whose value is the sum of the values of two expressions.
     */
    case class PlusExp (left : Exp, right : Exp) extends Exp

    /**
     * An expression whose value is the product of the values of two expressions.
     */
    case class StarExp (left : Exp, right : Exp) extends Exp

    /**
     * An expression whose value is the division of the values of two expressions.
     */
    case class SlashExp (left : Exp, right : Exp) extends Exp

    /**
     * An identifier reference.
     */
    sealed trait Idn extends FunLangNode {
        def idn : Identifier
    }

    /**
     * A defining occurrence (def) of an identifier.
     */
    case class IdnDef (idn : Identifier) extends Idn

    /**
     * An applied occurrence (use) of an identifier.
     */
    case class IdnUse (idn : Identifier) extends Exp with Idn

    /**
     * A representation of identifiers as strings.
     */
    type Identifier = String

}
