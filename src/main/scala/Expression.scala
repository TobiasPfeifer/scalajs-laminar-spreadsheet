import Spreadsheet.Cell
import Spreadsheet.Err
import com.raquo.airstream.core.Signal
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import Expression._
import fastparse._, NoWhitespace._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

sealed trait Expression[T] { self =>
  def eval(ctx: (Int, Int) => Option[Cell]): Signal[Expression.Result[T]] = Expression.compute(self, ctx)
}

object Expression {
  def parseFromString(exp: String, cellCoords: (Int, Int)): Either[Err, Expression[_]] = {
    def stringChars(c: Char)                      = c != '\"' && c != '\\'
    def strChars[_: P]                            = P(CharsWhile(stringChars))
    def escape[_: P]                              = P("\\" ~ (CharIn("\"/\\\\bfnrt")))
    def int[_: P]: P[Int]                         = P(CharIn("0-9").rep.!.map(_.toInt))
    def start[_: P]: P[Expression[_]]             = P((("=" ~ expr) | plain | empty) ~ End)
    def empty[_: P]: P[Expression[String]]        = P("").map(_ => Literal(""))
    def plain[_: P]: P[Expression[String]]        = P((CharPred(_ != '=').! ~ AnyChar.rep).!).map(Literal(_))
    def expr[_: P]: P[Expression[_]]              = P(exprString | exprNum | exprDateTime)
    def exprString[_: P]: P[Expression[String]]   = P(literal | cellref | concat)
    def exprNum[_: P]: P[Expression[BigDecimal]]  = P(subtract | sum | negate | mult | div | exprString.map(Num(_)))
    def exprDateTime[_: P]: P[Expression[String]] = now

    def literal[_: P]: P[Expression[String]]      = P("\"" ~/ (strChars | escape).rep.! ~ "\"").map(Literal(_))
    def cellref[_: P]: P[Expression[String]]      = P("[" ~ int ~ ":" ~ int ~ "]").map(t => CellRef(t._1, t._2))
    def concat[_: P]: P[Expression[String]]       = P("CONCAT(" ~ exprString ~ "," ~ exprString ~ ")").map(t => Concat(t._1, t._2))
    def subtract[_: P]: P[Expression[BigDecimal]] = P("SUBTRACT(" ~ exprNum ~ "," ~ exprNum ~ ")").map(t => Sum(t._1, Negate(t._2)))
    def sum[_: P]: P[Expression[BigDecimal]]      = P("SUM(" ~ exprNum ~ "," ~ exprNum ~ ")").map(t => Sum(t._1, t._2))
    def negate[_: P]: P[Expression[BigDecimal]]   = P("NEGATE(" ~ exprNum ~ ")").map(t => Negate(t))
    def mult[_: P]: P[Expression[BigDecimal]]     = P("MULT(" ~ exprNum ~ "," ~ exprNum ~ ")").map(t => Mult(t._1, t._2))
    def div[_: P]: P[Expression[BigDecimal]]      = P("DIV(" ~ exprNum ~ "," ~ exprNum ~ ")").map(t => Divide(t._1, t._2))
    def now[_: P]: P[Expression[String]]          = P("NOW()").map(_ => CurrentDateTime)

    parse(exp, start(_)) match {
      case f: Failure            => Left(Err(s"Error parsing at [${cellCoords._1}, ${cellCoords._2}]: ${f.trace().longMsg}"))
      case Success(value, index) => Right(value)
    }
  }

  def parseBigDecimal(s: String): Either[Err, BigDecimal] =
    try {
      if (s.isEmpty()) Left(Err(s"Error parsing number: empty string")) else Right(BigDecimal(s))
    } catch {
      case _: Throwable => Left(Err(s"Error parsing number: '${s}'"))
    }

  case class Literal(value: String)                                             extends Expression[String]
  case class CellRef(row: Int, column: Int)                                     extends Expression[String]
  case class Num(expr: Expression[_])                                           extends Expression[BigDecimal]
  case class Negate(num: Expression[BigDecimal])                                extends Expression[BigDecimal]
  case class Sum(num1: Expression[BigDecimal], num2: Expression[BigDecimal])    extends Expression[BigDecimal]
  case class Divide(num1: Expression[BigDecimal], num2: Expression[BigDecimal]) extends Expression[BigDecimal]
  case class Mult(num1: Expression[BigDecimal], num2: Expression[BigDecimal])   extends Expression[BigDecimal]
  case class Concat(expr1: Expression[String], expr2: Expression[String])       extends Expression[String]
  case object CurrentDateTime                                                   extends Expression[String]

  type Result[T] = Either[Spreadsheet.Err, T]
  def compute[A](expr: Expression[A], ctx: (Int, Int) => Option[Cell]): Signal[Result[A]] = {
    def combine[T](sigLeft: Signal[Result[T]], sigRight: Signal[Result[T]], combine: (T, T) => T) =
      sigLeft.combineWith(sigRight).map(t => t._1.flatMap(left => t._2.map(right => combine(left, right))))

    expr match {
      case Literal(value)       => Signal.fromValue(Right(value))
      case CellRef(row, column) => ctx(row, column).map(_.value).getOrElse(Signal.fromValue(Left(Err(s"invalid cell reference [$row:$column]"))))
      case Num(inner)           => inner.eval(ctx).map(v => v.flatMap(s => parseBigDecimal(s.toString())))
      case Negate(num)          => num.eval(ctx).map(_.map(_ * -1))
      case Sum(num1, num2)      => combine[BigDecimal](num1.eval(ctx), num2.eval(ctx), (num1, num2) => num1 + num2)
      case Divide(num1, num2)   => combine[BigDecimal](num1.eval(ctx), num2.eval(ctx), (num1, num2) => num1 / num2)
      case Mult(num1, num2)     => combine[BigDecimal](num1.eval(ctx), num2.eval(ctx), (num1, num2) => num1 * num2)
      case Concat(expr1, expr2) => combine[String](expr1.eval(ctx), expr2.eval(ctx), (s1, s2) => s1 + s2)
      case CurrentDateTime      => Signal.fromValue(Right(LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME)))
    }
  }
}
