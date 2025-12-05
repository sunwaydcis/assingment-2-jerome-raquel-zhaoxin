object Question1:
  case class Result(country: String, totalBookings: Int)

  def findTopBookingCountry(bookings: List[HotelBooking]): Result =
    val res =
      bookings
        .groupBy(_.destinationCountry)
        .view.mapValues(_.size)
        .maxByOption(_._2)
        .map(Result.apply)
        .getOrElse(Result("N/A", 0))

    res // return value
  end findTopBookingCountry
end Question1
