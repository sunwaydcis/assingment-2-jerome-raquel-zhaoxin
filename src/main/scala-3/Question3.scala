object Question3 {
  def calculateMostProfitableHotel(bookings: List[HotelBooking]): String =
    val hotelStats = bookings
      .groupBy(_.hotelName)
      .values
      .map { bookingList =>
        val visitorSum = bookingList.map(_.noOfVisitors).sum
        val visitorProfitMargin = visitorSum * bookingList.head.profitMargin

        (bookingList.head.hotelName, visitorProfitMargin)
      }
      .toList

    val mostProfitableHotel = hotelStats.maxBy(_._2)

    s"The most profitable hotel is ${mostProfitableHotel._1}"
  end calculateMostProfitableHotel
}