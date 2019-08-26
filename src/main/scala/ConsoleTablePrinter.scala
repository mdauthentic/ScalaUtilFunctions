class ConsoleTablePrinter {


  /**
   * Returns the data table
   * @param data: List of value (Any) to print inside a table
   * @return full table containing all rows/columns of data
   * */
  def displayTable(data: List[List[Any]]): String = data match {

    case List() => ""

    case _ =>
      val sizes = (data.map { e =>
        e.map(x => if (x == null) 0 else x.toString.length)
      }).transpose

      val colSizes = sizes.map(e => e.max)

      val rows = data.map(e => rowFormatter(e, colSizes))

      rowsBuilder(rowSeparator(colSizes), rows)
  }

  /**
   * Build table rows
   * @param rowSeparator: table cells separator
   * @param rows: row data
   * */
  private def rowsBuilder(rowSeparator: String, rows: List[String]): String = {
    val row = rowSeparator ::
      rows.head ::
      rowSeparator ::
      rows.tail :::
      rowSeparator ::
      List()

    row.mkString("\n")
  }

  private def rowFormatter(row: List[Any], colSizes: List[Int]): String = {

    val cells = (row.zip(colSizes)).collect {

      case (elem, size) => if (size == 0) "" else ("%" + size + "s").format(elem)

    }

    cells.mkString("|", "|", "|")
  }

  private def rowSeparator(colSizes: Seq[Int]): String = colSizes.map("-" * _).mkString("+", "+", "+")

}
