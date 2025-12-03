object Question1:
  case class Result(country: String, total: Int)

  def findTopBookingCountry(bookings: List[HotelBooking]): Result =
    val res =
      bookings
        .groupBy(_.destinationCountry)
        .mapValues(_.size)
        .maxByOption(_._2)
        .map(Result.apply)
        .getOrElse(Result("N/A", 0))

    println("[Country with Highest Number of Bookings]")
    println(s"- Country: ${res.country}")
    println(s"- Total Bookings: ${res.total}")

    res // return value

end Question1
