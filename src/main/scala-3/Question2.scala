object Question2 {

  case class EconomicalScore(
    hotelName: String,
    destinationCountry: String,
    destinationCity: String,
    averagePrice: Double,
    averageDiscount: Double,
    averageProfitMargin: Double
  )

  case class EconomicScorePercentage(
    hotelName: String,
    destinationCountry: String,
    destinationCity: String,
    score: Double
  )

  def calculateEconomicScore(bookings: List[HotelBooking]): Option[EconomicScorePercentage] =
    if bookings.isEmpty then return None

    val scores =
      bookings
        .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
        .map { case ((hotelName, destinationCountry, destinationCity), bs) =>

          val effective = bs.map(b => b.bookingPrice / b.rooms / b.noOfDays)

          EconomicalScore(
            hotelName,
            destinationCountry,
            destinationCity,
            averagePrice = effective.sum / effective.length,
            averageDiscount = bs.map(_.discount).sum / bs.length,
            averageProfitMargin = bs.map(_.profitMargin).sum / bs.length
          )
        }
        .toList

    val minP = scores.map(_.averagePrice).min
    val maxP = scores.map(_.averagePrice).max
    val rangeP = maxP - minP

    val minD = scores.map(_.averageDiscount).min
    val maxD = scores.map(_.averageDiscount).max
    val rangeD = maxD - minD

    val minPM = scores.map(_.averageProfitMargin).min
    val maxPM = scores.map(_.averageProfitMargin).max
    val rangePM = maxPM - minPM

    def safeNorm(x: Double, min: Double, range: Double): Double =
      if range == 0 then 0.0 else (x - min) / range

    val finals = scores.map { s =>
      val priceNorm = safeNorm(s.averagePrice, minP, rangeP)
      val discountNorm = safeNorm(s.averageDiscount, minD, rangeD)
      val profitNorm = safeNorm(s.averageProfitMargin, minPM, rangePM)

      val combined = (1 - priceNorm + discountNorm + 1 - profitNorm) / 3

      EconomicScorePercentage(s.hotelName, s.destinationCountry, s.destinationCity, combined)
    }


    Some(finals.maxBy(_.score))
  end calculateEconomicScore
}