/**
 * FunLang syntax analyser.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for FunLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import FunLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        ((exp <~ ";")+) ^^ Program

    lazy val exp : PackratParser[Exp] =
        ("let" ~> idndef) ~ ("=" ~> arg) ~ ("in" ~> exp) ^^ {
            case n ~ e ~ t => Fun (n, e, t)
        } |
        ("if" ~> exp) ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
            case cond ~ left ~ right =>
                IfExp (cond, left, right)
        } |
        ("(" ~> idndef) ~ (":" ~> tipe <~ ")") ~ ("=>" ~> exp) ^^ {
            case argName ~ argType ~ body => FunExp (argName, argType, body)
        } |
        rel

    lazy val rel : PackratParser[Exp] =
        app ~ ("=" ~> app) ^^ { case l ~ r => EqualExp (l, r) } |
        app ~ ("<" ~> app) ^^ { case l ~ r => LessExp (l, r) } |
        app

    lazy val app : PackratParser[Exp] =
        arith ~ app ^^ { case l ~ r => AppExp (l, r) } |
        arith

    lazy val arith : PackratParser[Exp] =
        arith ~ ("+" ~> star) ^^ { case l ~ r => PlusExp (l, r) } |
        arith ~ ("-" ~> star) ^^ { case l ~ r => MinusExp (l, r) } |
        star

    lazy val star : PackratParser[Exp] =
        star ~ ("*" ~> factor) ^^ { case l ~ r => StarExp (l, r) } |
        star ~ ("/" ~> factor) ^^ { case l ~ r => SlashExp (l, r) } |
        factor

    lazy val factor : PackratParser[Exp] =
        "false" ^^ (_ => BoolExp (false)) |
        "true" ^^ (_ => BoolExp (true)) |
        idnuse |
        integer ^^ (s => IntExp (s.toInt)) |
        "(" ~> exp <~ ")" |
        failure ("exp expected")

    // NOTE: the second lines for block, valdefn, fundefn, tipe need to be
    //       completely replaced; they are there just to keep the compiler quiet
    lazy val block : PackratParser[Exp] =
        "{" ^^^ BlockExp(Vector(), IntExp(0))  // FIXME

    lazy val valdefn : PackratParser[Val] =
        "val" ^^^ Val(IdnDef("fred"), IntExp(0)) // FIXME

    lazy val fundefn : PackratParser[Fun] =
        "def" ^^^ Fun(IdnDef("mary"), Arg(IdnDef("joe"), IntType()), IntExp(0)) // FIXME

    lazy val tipe : PackratParser[Type] =
        "some type" ^^^ IntType()

    // NOTE: You should not need to change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        identifier ^^ IdnDef

    lazy val idnuse =
        identifier ^^ IdnUse

    val keywordStrings =
        List ("def", "else", "false", "if", "then", "true", "val")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    override val whitespace : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "/*" ~ rep (not ("*/") ~ (comment | any)) ~ "*/" |
        "//.*\n".r

}
