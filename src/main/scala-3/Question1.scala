import scala.util.{Success, Failure}

object Question1:
  case class result(country: String, total: Int)

  def analyze(bookings: List[HotelBooking]): result =
    val grouped = bookings.groupBy(_.destinationCountry)
    if grouped.isEmpty then
      result("N/A", 0)
    else
      // maxBy finds the country with the largest number of bookings
      val(country, list) = grouped.maxBy(_._2.size)
      result(country, list.size)

  // Uses pattern matching on Success and Failure to ensure safe execution
  def run(): Unit =
    CSVLoader.loadHotelData() match
      case Success(bookings) =>
        val result = analyze(bookings)
        println("[Country with Highest Number of Bookings]")
        println(s"- Country: ${result.country}")
        println(s"- Total Bookings: ${result.total}")
      case Failure(ex) =>
        println("Failed to load CSV: " + ex.getMessage)

end Question1
