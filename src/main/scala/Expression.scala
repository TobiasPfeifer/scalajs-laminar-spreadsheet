import Spreadsheet.Cell
import Spreadsheet.Err
import com.raquo.airstream.core.Signal
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import Expression._
import fastparse._, NoWhitespace._

sealed trait Expression[T] { self =>
  def eval(ctx: (Int, Int) => Option[Cell]): Signal[Either[Spreadsheet.Err, T]]
}
object Expression {
  def parseFromString(exp: String, cellCoords: (Int, Int)): Either[Err, Expression[_]] = {
          def start[_: P]: P[Expression[_]]         = P((("=" ~ expr) | plain | empty) ~ End)
    def empty[_: P]: P[Literal]               = P("").map(_ => Literal(""))
    def plain[_: P]: P[Literal]               = P((CharPred(_ != '=').! ~ AnyChar.rep).!).map(Literal(_))
    def typedNum[_: P]: P[Expression[BigDecimal]] = P(sumNum | sum | mult)
    def typedStr[_: P]: P[Expression[String]] = P(literal | cellref | concat)
    def expr[_: P]: P[Expression[_]]          = P(typedStr | typedNum)

    def stringChars(c: Char)      = c != '\"' && c != '\\'
    def strChars[_: P]            = P(CharsWhile(stringChars))
    def escape[_: P]              = P("\\" ~ (CharIn("\"/\\\\bfnrt")))
    def literal[_: P]: P[Literal] = P("\"" ~/ (strChars | escape).rep.! ~ "\"").map(Literal(_))

    def int[_: P]: P[Int]         = P(CharIn("0-9").rep.!.map(_.toInt))
    def cellref[_: P]: P[CellRef] = P("[" ~ int ~ ":" ~ int ~ "]").map(t => CellRef(t._1, t._2))
    def concat[_: P]: P[Concat]   = P("CONCAT(" ~ expr ~ "," ~ expr ~ ")").map(t => Concat(t._1, t._2))
    def sumNum[_: P]: P[Sum]      = P("SUM(" ~ typedNum ~ "," ~ typedNum ~ ")").map(t => Sum(t._1, t._2))
    def sum[_: P]: P[Sum]         = P("SUM(" ~ expr ~ "," ~ expr ~ ")").map(t => Sum(Num(t._1), Num(t._2)))
    def mult[_: P]: P[Mult]         = P("MULT(" ~ expr ~ "," ~ expr ~ ")").map(t => Mult(Num(t._1), Num(t._2)))

    parse(exp, start(_)) match {
      case f: Failure            => Left(Err(s"Error parsing at [${cellCoords._1}, ${cellCoords._2}]: ${f.msg}"))
      case Success(value, index) => Right(value)
    }
  }

  def parseBigDecimal(s: String): Either[Err, BigDecimal] = {
      try {
          if (s.isEmpty()) Left(Err(s"Error parsing number: empty string"))
          else Right(BigDecimal(s))
      } catch {
        case _: Throwable => Left(Err(s"Error parsing number: '${s}'"))
      }
  }

  case class Literal(value: String) extends Expression[String] {
    override def eval(ctx: (Int, Int) => Option[Cell]): Signal[Either[Err, String]] = Signal.fromValue(Right(value))
  }
  case class CellRef(row: Int, column: Int) extends Expression[String] {
    override def eval(ctx: (Int, Int) => Option[Cell]): Signal[Either[Err, String]] =
      ctx(row, column) match {
        case Some(cell) => cell.value
        case None       => Signal.fromValue(Left(Err(s"invalid cell reference [$row:$column]")))
      }
  }
  case class Num(expr: Expression[_]) extends Expression[BigDecimal] {
          override def eval(ctx: (Int, Int) => Option[Cell]): Signal[Either[Err, BigDecimal]] = expr
      .eval(ctx)
      .map(v => v.flatMap(s => parseBigDecimal(s.toString)))
  }
  case class Sum(num1: Expression[BigDecimal], num2: Expression[BigDecimal]) extends Expression[BigDecimal] {
    override def eval(ctx: (Int, Int) => Option[Cell]): Signal[Either[Err, BigDecimal]] =
              num1.eval(ctx).combineWith(num2.eval(ctx)).map(t => t._1.flatMap(ld => t._2.map(ld + _)))
  }

  case class Concat(expr1: Expression[_], expr2: Expression[_]) extends Expression[String] {
    override def eval(ctx: (Int, Int) => Option[Cell]): Signal[Either[Err, String]] =
      expr1.eval(ctx).combineWith(expr2.eval(ctx)).map(t => t._1.flatMap(s => t._2.map(s.toString + _.toString())))
  }

  case class Mult(num1: Expression[BigDecimal], num2: Expression[BigDecimal]) extends Expression[BigDecimal] {
   override def eval(ctx: (Int, Int) => Option[Cell]): Signal[Either[Err, BigDecimal]] =
              num1.eval(ctx).combineWith(num2.eval(ctx)).map(t => t._1.flatMap(ld => t._2.map(ld * _)))
  }
}
