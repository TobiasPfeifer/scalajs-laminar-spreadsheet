import org.scalajs.dom
import com.raquo.laminar.api.L._


object Main {
    lazy val container = dom.document.getElementById("app-container")
    lazy val rootElement = Spreadsheet.$view

    def main(args: Array[String]): Unit = {
        renderOnDomContentLoaded(container, rootElement)
    }
}