import org.scalajs.dom.raw.HTMLElement

import com.raquo.laminar.api.L._
import com.raquo.airstream.core
import app.tulz.tuplez.Composition
import Expression._

object Spreadsheet {
  type CellGrid = List[List[Cell]]

  lazy val grid: CellGrid = buildDemoGrid()

  def buildDemoGrid() = {
    lazy val cells: CellGrid = (0 to 20).toList.map(row => (0 to 5).toList.map(col => Cell(() => cells, Var(""), row -> col)))
    cells(0)(0).expression.set("Item name")
    cells(1)(0).expression.set("Apple")
    cells(2)(0).expression.set("Egg")
    cells(3)(0).expression.set("Milk")
    cells(0)(1).expression.set("Price per unit")
    cells(1)(1).expression.set("1.69")
    cells(2)(1).expression.set("3.99")
    cells(3)(1).expression.set("1.29")
    cells(0)(2).expression.set("Amounts")
    cells(1)(2).expression.set("2")
    cells(2)(2).expression.set("3")
    cells(3)(2).expression.set("4")
    cells(0)(3).expression.set("Total")
    cells(1)(3).expression.set("=MULT([1:1],[1:2])")
    cells(2)(3).expression.set("=MULT([2:1],[2:2])")
    cells(3)(3).expression.set("=MULT([3:1],[3:2])")
    cells(4)(3).expression.set("=SUM(SUM([1:3],[2:3]),[3:3])")

    cells(8)(0).expression.set("=\"hello\"")
    cells(8)(1).expression.set("=CONCAT([8:0],\" scala!\")")
    cells(8)(2).expression.set("=CONCAT([8:0],\" laminar!\")")
    cells(8)(3).expression.set("=CONCAT([8:0],\" world!\")")
    cells
  }
  val editVar: Var[(Option[(Int, Int)], Var[String])] = Var((None, Var("")))

  val $view: Div =
    div(
      h1("Hello world"),
      child <-- editVar.signal.map(_._1).map(_.fold("Input: []")(t => s"Input [${t._1}:${t._2}] ")).map(span(_)),
      input(
        label("Input"),
        width("80%"),
        disabled <-- editVar.signal.map(_._1.isEmpty),
        controlled(
          value <-- editVar.signal.flatMap(_._2.signal),
          onInput.mapToValue --> editVar.writer.contramap { (v: String) =>
            editVar.now._2.set(v); (editVar.now()._1, editVar.now()._2)
          }
        )
      ),
      hr(marginTop("1em"), marginBottom("1em")),
      div(
        overflow("scroll"),
        grid.map(renderRow(_))
      )
    )

  def renderRow(columns: List[Cell]): Div =
    div(
      display("flex"),
      flexDirection("row"),
      columns.map(renderCell(_))
    )

  def renderCell(cell: Cell): Div =
    div(
      display("flex"),
      flexDirection("column"),
      input(
        borderWidth("thin"),
        padding("0.1em"),
        boxSizing("border-box"),
        border("1px solid #ccc"),
        transition("0.2s"),
        outline("none"),
        fontSize("large"),
        readOnly(true),
        value <-- cell.value.map(_.fold(_.msg, _.toString())),
        onFocus --> editVar.writer.contramap((_: Any) => Some(cell.coords) -> cell.expression)
      )
    )

  case class Err(msg: String)

  case class Cell(cells: () => CellGrid, expression: Var[String], coords: (Int, Int)) {
    val ctx: (Int, Int) => Option[Cell] = (row, col) => cells().lift(row).flatMap(_.lift(col))
    lazy val value: Signal[Either[Err, String]] =
      expression.signal
        .map(Expression.parseFromString(_, coords))
        .map(_.map(_.eval(ctx)))
        .flatMap(_ match {
          case l @ Left(_)   => Signal.fromValue(l)
          case Right(signal) => signal
        })
        .map(_.map(_.toString()))
  }
}
