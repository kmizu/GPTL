import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._

object Token {
  sealed trait Token

  case class IntToken(value: BigInt) extends Token

  case class StrToken(value: String) extends Token

  case object TrueToken extends Token

  case object FalseToken extends Token

  case class IdentifierToken(name: String) extends Token

  case class SymbolToken(op: String) extends Token

  case class KeywordToken(kw: String) extends Token
}

object Expr {
  import Token._
  sealed trait Expr
  case class IntLiteral(value: BigInt) extends Expr
  case class StrLiteral(value: String) extends Expr
  case class BoolLiteral(value: Boolean) extends Expr
  case class VarRef(name: String) extends Expr
  case class Let(name: String, value: Expr, body: Expr) extends Expr
  case class If(cond: Expr, trueExpr: Expr, falseExpr: Expr) extends Expr
  case class While(cond: Expr, body: Expr) extends Expr
  case class BinaryOp(op: String, left: Expr, right: Expr) extends Expr
  case class UnaryOp(op: String, expr: Expr) extends Expr
  case class Block(exprs: List[Expr]) extends Expr
  case class AssignmentExpr(variable: String, value: Expr) extends Expr
}

import Token._
import Expr._

object Tokenizer {
  private val tokenPattern: Regex =
    raw"""(-?\d+)|("(?:[^"\\]|\\.)*")|(true|false)|([a-zA-Z][a-zA-Z0-9]*)|(:=)|([+\-*/<>!=;])|(and|or|not)|([(){}])|(\s+)|(let|in|if|then|else|while|do)""".r

  def tokenize(input: String): List[Token] = {
    tokenPattern.findAllIn(input).toList.flatMap {
      case t if t.matches(raw"""".*"""") => List(StrToken(t.drop(1).dropRight(1)))
      case "true" => List(TrueToken)
      case "false" => List(FalseToken)
      case t if t.matches(raw"""\d+""") => List(IntToken(BigInt(t)))
      case t if t.matches(raw"""[+\-*/<>!=(){}]|:=|;""") => List(SymbolToken(t))
      case t if t.matches(raw"""(and|or|not|let|in|if|else|then|while|do)""") => List(KeywordToken(t))
      case t if t.matches(raw"""[a-zA-Z][a-zA-Z0-9]*""") => List(IdentifierToken(t))
      case t if t.matches(raw"""\s+""") => Nil
      case t => throw new Exception(s"Invalid token: $t")
    }
  }
}

object Parser extends Parsers {
  override type Elem = Token
  def parse(tokens: List[Token]): Expr = {
    val reader = new TokenReader(tokens)
    expr(reader) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) => throw new Exception(s"Parse error: $msg")
    }
  }

  private def expr: Parser[Expr] = assignmentExpr | letExpr | ifExpr | whileExpr | blockExpr | logicalExpr

  private def assignmentExpr: Parser[AssignmentExpr] = identifier ~ SymbolToken(":=") ~ expr ^^ {
    case variable ~ _ ~ value => AssignmentExpr(variable.asInstanceOf[VarRef].name, value)
  }

  private def letExpr: Parser[Expr] = (
    (KeywordToken("let") ~> identifier <~ SymbolToken("=")) ~ expr ~ (KeywordToken("in") ~> expr) ^^ {
      case name ~ value ~ body => Let(name.name, value, body)
    }
  )

  private def ifExpr: Parser[Expr] = (
    (KeywordToken("if") ~> expr <~ KeywordToken("then")) ~ expr ~ (KeywordToken("else") ~> expr) ^^ {
      case cond ~ trueExpr ~ falseExpr => If(cond, trueExpr, falseExpr)
    }
  )

  private def whileExpr: Parser[Expr] = KeywordToken("while") ~> expr ~ (KeywordToken("do") ~> expr) ^^ {
    case cond ~ body => While(cond, body)
  }

  private def blockExpr: Parser[Expr] = SymbolToken("{") ~> repsep(expr, SymbolToken(";")) <~ SymbolToken("}") ^^ {
    exprs => Block(exprs)
  }

  private def logicalExpr: Parser[Expr] = additiveExpr ~ rep(
    (SymbolToken("<") | SymbolToken("<=") | SymbolToken(">") | SymbolToken(">=") | SymbolToken("==") | SymbolToken("!=")) ~ additiveExpr
  ) ^^ {
    case t ~ ts => ts.foldLeft(t) { case (left, op ~ right) => BinaryOp(op.asInstanceOf[SymbolToken].op, left, right) }
  }

  private def additiveExpr: Parser[Expr] = multiplicativeExpr ~ rep(
    (SymbolToken("+") | SymbolToken("-")) ~ multiplicativeExpr
  ) ^^ {
    case t ~ ts => ts.foldLeft(t) { case (left, op ~ right) => BinaryOp(op.asInstanceOf[SymbolToken].op, left, right) }
  }

  private def multiplicativeExpr: Parser[Expr] = term ~ rep(
    (SymbolToken("*") | SymbolToken("/")) ~ term
  ) ^^ {
    case t ~ ts => ts.foldLeft(t) { case (left, op ~ right) => BinaryOp(op.asInstanceOf[SymbolToken].op, left, right) }
  }

  private def term: Parser[Expr] = (
      intLiteral
    | strLiteral
    | boolLiteral
    | identifier
    | SymbolToken("(") ~> expr <~ SymbolToken(")")
  )

  private def intLiteral: Parser[Expr] = accept("integer", { case IntToken(n) => IntLiteral(n) })

  private def strLiteral: Parser[Expr] = accept("string", { case StrToken(s) => StrLiteral(s) })

  private def boolLiteral: Parser[Expr] = (TrueToken ^^^ BoolLiteral(true)) | (FalseToken ^^^ BoolLiteral(false))

  private def identifier: Parser[VarRef] = accept("identifier", { case IdentifierToken(name) => VarRef(name) })

  private class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }
}
object Interpreter {
  import scala.collection.mutable
   type Environment = mutable.Map[String, Any]
  def eval(expr: Expr, env: Environment = mutable.Map.empty): Any = expr match {
    case IntLiteral(value) => value
    case StrLiteral(value) => value
    case BoolLiteral(value) => value
    case VarRef(name) => env.getOrElse(name, throw new Exception(s"Undefined variable: $name"))
    case AssignmentExpr(variable, value) =>
      val newValue = eval(value, env)
      env.put(variable, newValue)
      newValue
    case BinaryOp(op, left, right) => op match {
      case "+" => eval(left, env).asInstanceOf[BigInt] + eval(right, env).asInstanceOf[BigInt]
      case "-" => eval(left, env).asInstanceOf[BigInt] - eval(right, env).asInstanceOf[BigInt]
      case "*" => eval(left, env).asInstanceOf[BigInt] * eval(right, env).asInstanceOf[BigInt]
      case "/" => eval(left, env).asInstanceOf[BigInt] / eval(right, env).asInstanceOf[BigInt]
      case "<" => eval(left, env).asInstanceOf[BigInt] < eval(right, env).asInstanceOf[BigInt]
      case "<=" => eval(left, env).asInstanceOf[BigInt] <= eval(right, env).asInstanceOf[BigInt]
      case ">" => eval(left, env).asInstanceOf[BigInt] > eval(right, env).asInstanceOf[BigInt]
      case ">=" => eval(left, env).asInstanceOf[BigInt] >= eval(right, env).asInstanceOf[BigInt]
      case "==" => eval(left, env) == eval(right, env)
      case "!=" => eval(left, env) != eval(right, env)
      case "and" => eval(left, env).asInstanceOf[Boolean] && eval(right, env).asInstanceOf[Boolean]
      case "or" => eval(left, env).asInstanceOf[Boolean] || eval(right, env).asInstanceOf[Boolean]
    }
    case UnaryOp(op, expr) => op match {
      case "not" => !eval(expr, env).asInstanceOf[Boolean]
    }
    case Block(exprs) => exprs.foldLeft[Any](()) { case (_, e) => eval(e, env) }
    case If(cond, trueBranch, falseBranch) =>
      if (eval(cond, env).asInstanceOf[Boolean]) eval(trueBranch, env)
      else eval(falseBranch, env)
    case Let(variable, value, body) =>
      val newValue = eval(value, env)
      env.put(variable, newValue)
      eval(body, env)
    case While(cond, body) =>
      var result: Any = 0
      while (eval(cond, env).asInstanceOf[Boolean]) {
        result = eval(body, env)
      }
      result
  }
}

import Tokenizer._
import Parser._
import Interpreter._
import scala.collection.mutable
object Main extends App {
  val tokens = tokenize("""
    let i = 0 in {
      while i < 10 do {
        i := i + 1
      };
      i
    }
    """)
  println("tokens: " + tokens)
  val expression = parse(tokens)
    println("expression: " + expression)
  val result = eval(expression, mutable.Map.empty)
    println("result: " + result)
}


