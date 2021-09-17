import Spreadsheet.Cell
import Spreadsheet.Err
import com.raquo.airstream.core.Signal
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import Expression._
import fastparse._, NoWhitespace._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

sealed trait Expression { self =>
  type Result[T] = Either[Spreadsheet.Err, T]

  def eval(ctx: (Int, Int) => Option[Cell]): Signal[Result[String]] = {
    def combine[T](sigLeft: Signal[Result[T]], sigRight: Signal[Result[T]], combine: (T, T) => T) =
      sigLeft.combineWith(sigRight).map(t => t._1.flatMap(left => t._2.map(right => combine.apply(left, right))))
    def asString[T](signal: Signal[Result[T]]): Signal[Result[String]] = signal.map(_.map(_.toString()))

    self match {
      case Literal(value)       => Signal.fromValue(Right(value))
      case CellRef(row, column) => ctx(row, column).map(_.value).getOrElse(Signal.fromValue(Left(Err(s"invalid cell reference [$row:$column]"))))
      case n @ Num(expr)        => n.evalNum(ctx).map(_.map(_.toString()))
      case Negate(num)          => num.evalNum(ctx).map(_.map(-1 * _).map(_.toString()))
      case Concat(expr1, expr2) => asString(combine[String](expr1.eval(ctx), expr2.eval(ctx), (s1, s2) => s1 + s2))
      case Sum(num1, num2)      => asString(combine[BigDecimal](num1.evalNum(ctx), num2.evalNum(ctx), (num1, num2) => num1 + num2))
      case Divide(num1, num2)   => asString(combine[BigDecimal](num1.evalNum(ctx), num2.evalNum(ctx), (num1, num2) => num1 / num2))
      case Mult(num1, num2)     => asString(combine[BigDecimal](num1.evalNum(ctx), num2.evalNum(ctx), (num1, num2) => num1 * num2))
      case CurrentDateTime      => Signal.fromValue(Right(LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME)))
    }
  }
}
object Expression {
  def parseFromString(exp: String, cellCoords: (Int, Int)): Either[Err, Expression] = {
    def stringChars(c: Char)       = c != '\"' && c != '\\'
    def strChars[_: P]             = P(CharsWhile(stringChars))
    def escape[_: P]               = P("\\" ~ (CharIn("\"/\\\\bfnrt")))
    def int[_: P]: P[Int]          = P(CharIn("0-9").rep.!.map(_.toInt))
    def start[_: P]: P[Expression] = P((("=" ~ expr) | plain | empty) ~ End)
    def empty[_: P]: P[Expression] = P("").map(_ => Literal(""))
    def plain[_: P]: P[Expression] = P((CharPred(_ != '=').! ~ AnyChar.rep).!).map(Literal(_))
    def expr[_: P]: P[Expression]  = P(literal | cellref | concat | subtract | sum | negate | mult | div | now)

    def literal[_: P]: P[Expression]  = P("\"" ~/ (strChars | escape).rep.! ~ "\"").map(Literal(_))
    def cellref[_: P]: P[Expression]  = P("[" ~ int ~ ":" ~ int ~ "]").map(t => CellRef(t._1, t._2))
    def concat[_: P]: P[Expression]   = P("CONCAT(" ~ expr ~ "," ~ expr ~ ")").map(t => Concat(t._1, t._2))
    def subtract[_: P]: P[Expression] = P("SUBTRACT(" ~ expr ~ "," ~ expr ~ ")").map(t => Sum(Num(t._1), Num(Negate(Num(t._2)))))
    def sum[_: P]: P[Expression]      = P("SUM(" ~ expr ~ "," ~ expr ~ ")").map(t => Sum(Num(t._1), Num(t._2)))
    def negate[_: P]: P[Expression]   = P("NEGATE(" ~ expr ~ ")").map(t => Negate(Num(t)))
    def mult[_: P]: P[Expression]     = P("MULT(" ~ expr ~ "," ~ expr ~ ")").map(t => Mult(Num(t._1), Num(t._2)))
    def div[_: P]: P[Expression]      = P("DIV(" ~ expr ~ "," ~ expr ~ ")").map(t => Divide(Num(t._1), Num(Num(t._2))))
    def now[_: P]: P[Expression]      = P("NOW()").map(_ => CurrentDateTime)

    parse(exp, start(_)) match {
      case f: Failure            => Left(Err(s"Error parsing at [${cellCoords._1}, ${cellCoords._2}]: ${f.trace().longMsg}"))
      case Success(value, index) => Right(value)
    }
  }

  def parseBigDecimal(s: String): Either[Err, BigDecimal] =
    try {
      if (s.isEmpty()) Left(Err(s"Error parsing number: empty string"))
      else Right(BigDecimal(s))
    } catch {
      case _: Throwable => Left(Err(s"Error parsing number: '${s}'"))
    }

  case class Literal(value: String)         extends Expression
  case class CellRef(row: Int, column: Int) extends Expression
  case class Num(expr: Expression) extends Expression {
    def evalNum(ctx: (Int, Int) => Option[Cell]): Signal[Either[Err, BigDecimal]] = expr
      .eval(ctx)
      .map(v => v.flatMap(s => parseBigDecimal(s)))
  }
  case class Negate(num: Expression.Num)                        extends Expression
  case class Sum(num1: Expression.Num, num2: Expression.Num)    extends Expression
  case class Divide(num1: Expression.Num, num2: Expression.Num) extends Expression
  case class Mult(num1: Expression.Num, num2: Expression.Num)   extends Expression
  case class Concat(expr1: Expression, expr2: Expression)       extends Expression
  case object CurrentDateTime                                   extends Expression
}
