import Utils.*

object Question3 extends Question:

  case class HotelPerformance(
     hotelName: String,
     hotelCountry: String,
     hotelCity: String,
     averageVisitors: Double,
     averageProfitMargin: Double
   )

  case class HotelScore(
     hotelName: String,
     hotelCountry: String,
     hotelCity: String,
     score: Double
   )

  def calculateHotelPerformance(bookings: List[HotelBooking]): Option[HotelScore] =
    if bookings.isEmpty then None

    val stats =
      groupByHotel(bookings).map { case ((hotelName, country, city), bs) =>

        val avgVisitors =
          bs.map(_.noOfVisitors).sum.toDouble / bs.length

        val avgProfitMargin =
          bs.map(_.profitMargin).sum / bs.length

        HotelPerformance(
          hotelName,
          country,
          city,
          avgVisitors,
          avgProfitMargin
        )
      }.toList

    val (minV, maxV) = minMax(stats)(_.averageVisitors)
    val (minP, maxP) = minMax(stats)(_.averageProfitMargin)

    val finalScores = stats.map { s =>
      val visitorsNorm = safeNorm(s.averageVisitors,    minV, maxV)
      val profitNorm   = safeNorm(s.averageProfitMargin,minP, maxP)

      val combined = (visitorsNorm + profitNorm) / 2

      HotelScore(s.hotelName, s.hotelCountry, s.hotelCity, combined)
    }

    Some(finalScores.maxBy(_.score))
  end calculateHotelPerformance
  
  def printAnswer(bookings: List[HotelBooking]): Unit =
    calculateHotelPerformance(bookings) match
      case Some(best) =>
        println(s"The highest performing hotel is ${best.hotelName} located in ${best.hotelCity}, ${best.hotelCountry}.")
      case None =>
        println("No bookings available to calculate the highest performing hotel.")
  end printAnswer
end Question3
