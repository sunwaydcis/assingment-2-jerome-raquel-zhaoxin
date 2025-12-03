object Question2 {
  case class EconomicalScore(
    hotelName: String,
    avgPrice: Double,
    avgDiscount: Double,
    avgProfitMargin: Double,
    economicalScore: Double
  )

  def calculateEconomicScore(bookings: List[HotelBooking]): Option[EconomicalScore] =
    val hotelStats = bookings
      .groupBy(_.hotelName)
      .values
      .map { bookingList =>
        val totalRooms = bookingList.map(_.rooms).sum
        val totalBookingPrice = bookingList.map(_.bookingPrice).sum

        val avgPrice = totalBookingPrice / totalRooms
        val avgDiscount = bookingList.map(b => b.discount * b.bookingPrice).sum / totalBookingPrice
        val avgProfitMargin = bookingList.map(b => b.profitMargin * b.bookingPrice).sum / totalBookingPrice

        val finalPrice = avgPrice * (1 - avgDiscount / 100)
        val economicalScore = finalPrice * (1 + avgProfitMargin / 100)

        EconomicalScore(
          bookingList.head.hotelName,
          avgPrice,
          avgDiscount,
          avgProfitMargin,
          economicalScore
        )
      }
      .toList

    hotelStats.minByOption(_.economicalScore)
  end calculateEconomicScore
}
