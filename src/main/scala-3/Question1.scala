object Question1 extends Question:
  case class Result(country: String, totalBookings: Int)

  def findTopBookingCountry(bookings: List[HotelBooking]): Result =
    bookings
      .groupBy(_.destinationCountry)
      .map { case (country, list) => Result(country, list.length) }
      .maxByOption(_.totalBookings)
      .getOrElse(Result("N/A", 0))
  end findTopBookingCountry
  
  def printAnswer(bookings: List[HotelBooking]): Unit =
    val top = findTopBookingCountry(bookings)
    println(s"The country with the most bookings is ${top.country} with ${top.totalBookings} bookings.")
  end printAnswer
end Question1
