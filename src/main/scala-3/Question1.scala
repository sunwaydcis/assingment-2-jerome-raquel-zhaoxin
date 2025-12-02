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

  def run(bookings: List[HotelBooking]): Unit =
      val result = analyze(bookings)
      println("[Country with Highest Number of Bookings]")
      println(s"- Country: ${result.country}")
      println(s"- Total Bookings: ${result.total}")

end Question1
