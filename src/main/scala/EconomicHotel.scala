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
    .view
    .mapValues { bookingList =>
      val avgPrice = bookingList.map(_.bookingPrice).sum / bookingList.size
      val avgDiscount = bookingList.map(_.discount).sum / bookingList.size
      val avgProfitMargin = bookingList.map(_.profitMargin).sum / bookingList.size

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
    .values
    .toList

  hotelStats.minByOption(_.economicalScore)
end calculateEconomicScore