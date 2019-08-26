import scala.collection.mutable

class Percentiles {

  /**
   * Percentile calculator
   * @param xs: a list of double
   * @param percentiles: comma separated percentile value
   * @return map of percentile names and values
   *
   * @example percentiles(List(1.0, 20.0), 0.25, 0.50, 0.75)
   * */
  def percentiles(xs: List[Double], percentiles: Double*): mutable.Map[String, Double] = {

    val values = new Array[Double](percentiles.length)
    var i = 0
    val numbers = xs.sorted
    val percentileMap = mutable.Map[String, Double]()

    while (i < percentiles.length) {

      val index = (percentiles(i) * numbers.length).toInt
      values(i) = numbers(index)
      val percentileKey = (percentiles(i) * 100).toInt + "th"
      percentileMap ++= Map(percentileKey -> numbers(index))

      i += 1; i - 1
    }

    percentileMap

  }

  /**
   * Calculate the median of a list of values
   * @param xs: list of values
   * @return the median value
   * */
  def median(xs: List[Double]): Double = {
    val numbers = xs.sorted
    val middle = numbers.length / 2
    if (numbers.length % 2 == 1) numbers(middle)
    else (numbers(middle - 1) / 2.0) + (numbers(middle) / 2.0)
  }

  /**
   * Calculate the average of a list of values
   * @param xs: list of values (of any type; Int, Double, Long etc.)
   *
   * @example List(1.0, 20.0).avg
   * */
  def average[T](xs: Iterable[T])(implicit number: Numeric[T]): Double = number.toDouble(xs.sum) / xs.size
  implicit class IterableAvg[T: Numeric](data: Iterable[T]) { def avg: Double = average(data) }

}
