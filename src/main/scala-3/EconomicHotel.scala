case class EconomicalScore(
  hotelName: String,
  avgPrice: Double,
  avgDiscount: Double,
  avgProfitMargin: Double,
  economicalScore: Double
)

def calculateHotelStats(bookings: List[HotelBooking]): Map[String, (Double, Double, Double)] =
  bookings
    .groupBy(_.hotelName)
    .map { case (hotel, list) =>
      val avgPrice        = list.map(_.bookingPrice).sum / list.size
      val avgDiscount     = list.map(_.discount).sum / list.size
      val avgProfitMargin = list.map(_.profitMargin).sum / list.size
      hotel -> (avgPrice, avgDiscount, avgProfitMargin)
    }
end calculateHotelStats

def computeEconomicalScore(price: Double, discount: Double, margin: Double): Double =
  val finalPrice = price * (1 - discount / 100)
  finalPrice * (1 + margin / 100)
end computeEconomicalScore

def calculateEconomicScore(bookings: List[HotelBooking]): Option[EconomicalScore] =
  val stats = calculateHotelStats(bookings)

  stats
    .map { case (hotel, (avgP, avgD, avgM)) =>
      EconomicalScore(
        hotel,
        avgP,
        avgD,
        avgM,
        computeEconomicalScore(avgP, avgD, avgM)
      )
    }
    .minByOption(_.economicalScore)
end calculateEconomicScore
