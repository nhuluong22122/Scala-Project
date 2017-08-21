package system
//Ework functions: add, sub, mul, div, less than , equal, not
//Parser: take string an produce output
//Combinator = parser = parser1 |(combinator) parser2 
//if wants SUM to have lower precedence than Product ->  Sum = Product + Product 

import scala.util.parsing.combinator._ // have to download it online
import expression._
import value._
class EwokParsers extends RegexParsers {

  //EXPRESSION ::= DECLARATION | CONDITIONAL | DISJUNCTION
  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  //DECLARATION ::= "def"~identifier~"="~expression
  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
  }

  //CONDITIONAL ::= if~(~EXPRESSION~)~EXPRESSION~(else~EXPRESSION)?
  def conditional: Parser[Expression] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if" ~ "(" ~ condition ~ ")" ~ execution1 ~ None                      => Conditional(condition, execution1, null)
    case "if" ~ "(" ~ condition ~ ")" ~ execution1 ~ Some("else" ~ execution2) => Conditional(condition, execution1, execution2)
  }

  //DISJUNCTION ::= CONJUNCTION~(||~CONJUNCTION)*
  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ // rep = combinator -> return list of trees
    {
      case con ~ Nil  => con
      case con ~ cons => Disjunction(con :: cons) // 1 :: [2,3,4] = [1,2,3,4] 
    }

  //CONJUNCTION ::= EQUALITY~(&&~EQUALITY)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ { // Parser(input = String) => result type = Expression
    case eq1 ~ Nil    => eq1
    case eq1 ~ eqlist => Conjunction(eqlist.::(eq1))
  }

  //EQUALITY ::= INEQUALITY~(==~INEQUALITY)*
  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case i ~ Nil  => i
    case i ~ rest => new FunCall(Identifier("equals"), i :: rest)
  }

  //INEQUALITY ::= sum ~ ("<" ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt("<" ~ sum) ^^ {
    case sum ~ None             => sum
    case sum ~ Some("<" ~ sum2) => { val list = List(sum, sum2); FunCall(Identifier("less"), list) }
  }

  //SUM ::= product ~ (("+" | "-") ~ product)*
  def sum: Parser[Expression] =
    product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
      case p ~ Nil  => p
      case p ~ rest => { FunCall(Identifier("add"), p :: rest) }
    }

  //PRODUCT ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] =
    term ~ rep(("*" | "/") ~ term ^^ { case "*" ~ s => s case "/" ~ s => invert(s) }) ^^ {
      case p ~ Nil  => p
      case p ~ rest => { FunCall(Identifier("mul"), p :: rest) }
    }

  //FUNCALL ::= identifier~operands
  def funcall: Parser[Expression] = identifier ~ operands ^^ {
    case t ~ ops => FunCall(Identifier(term.toString()), ops);
  }

  //OPERANDS ::= (~(EXPRESSION~(,~EXPRESSION)*)?)
  def operands: Parser[List[Expression]] = "(" ~ opt(expression ~ rep("," ~> expression)) ~ ")" ^^ {
    case "(" ~ None ~ ")"                => Nil
    case "(" ~ Some(exp ~ Nil) ~ ")"     => exp :: Nil
    case "(" ~ Some(exp ~ explist) ~ ")" => exp :: explist
  }

  //TERM ::= funcall | identifier | number | boole | "("~expression~")"

  def term: Parser[Expression] = boole | funcall | identifier | number | "(" ~ expression ~ ")" ^^ {
    case "(" ~ exp ~ ")" => exp
  }
  //LITERAL ::= BOOLE | NUMBER
  def literal: Parser[Expression] = boole | number

  //IDENTIFIER ::= [a-zA-Z][0-9a-zA-Z]*
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case someString => { Identifier(someString) }
  }

  //NUMBER ::= (\+|-)?[0-9]+(\.[0-9]+)?
  def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ {
    case someString => { Number(someString.toDouble) }
  }

  //BOOLE ::= true | false
  def boole: Parser[Boole] = """true|false""".r ^^ {
    case s => { new Boole(s.toBoolean); }
  }

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = new Number(0)
    new FunCall(sub, List(zero, exp))
  }
  def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val zero = new Number(1)
    new FunCall(div, List(zero, exp))
  }
}