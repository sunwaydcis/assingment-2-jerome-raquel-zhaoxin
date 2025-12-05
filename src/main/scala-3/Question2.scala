import Utils.*

object Question2:

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
    if bookings.isEmpty then None

    val scores =
      groupByHotel(bookings).map { case ((hotelName, country, city), bs) =>

        val effectivePrices = bs.map(b => b.bookingPrice / b.rooms / b.noOfDays)

        EconomicalScore(
          hotelName,
          country,
          city,
          averagePrice        = effectivePrices.sum / effectivePrices.length,
          averageDiscount     = bs.map(_.discount).sum / bs.length,
          averageProfitMargin = bs.map(_.profitMargin).sum / bs.length
        )
      }.toList

    val (minP, maxP)  = minMax(scores)(_.averagePrice)
    val (minD, maxD)  = minMax(scores)(_.averageDiscount)
    val (minPM, maxPM)= minMax(scores)(_.averageProfitMargin)

    val finalScores = scores.map { s =>
      val priceNorm   = safeNorm(s.averagePrice,        minP,  maxP)
      val discountNorm= safeNorm(s.averageDiscount,     minD,  maxD)
      val profitNorm  = safeNorm(s.averageProfitMargin, minPM, maxPM)

      val combined =
        (1 - priceNorm + discountNorm + 1 - profitNorm) / 3

      EconomicScorePercentage(
        s.hotelName,
        s.destinationCountry,
        s.destinationCity,
        combined
      )
    }

    Some(finalScores.maxBy(_.score))
  end calculateEconomicScore
end Question2