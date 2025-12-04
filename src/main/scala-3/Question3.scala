object Question3 {

  case class HotelPerformance(
                               hotelName: String,
                               averageVisitors: Double,
                               averageProfitMargin: Double
                             )

  case class HotelScore(
                         hotelName: String,
                         score: Double
                       )

  def calculateHotelPerformance(bookings: List[HotelBooking]): Option[HotelScore] =
    if bookings.isEmpty then return None

    // Step 1: group bookings by hotel and calculate averages
    val stats = bookings.groupBy(_.hotelName).map { case (hotelName, bs) =>
      val avgVisitors = bs.map(_.noOfVisitors).sum.toDouble / bs.length
      val avgProfitMargin = bs.map(_.profitMargin).sum.toDouble / bs.length

      HotelPerformance(hotelName, avgVisitors, avgProfitMargin)
    }.toList

    // Step 2: find min/max and ranges for normalization
    val minVisitors = stats.map(_.averageVisitors).min
    val maxVisitors = stats.map(_.averageVisitors).max
    val rangeVisitors = maxVisitors - minVisitors

    val minProfit = stats.map(_.averageProfitMargin).min
    val maxProfit = stats.map(_.averageProfitMargin).max
    val rangeProfit = maxProfit - minProfit

    def safeNorm(x: Double, min: Double, range: Double): Double =
      if range == 0 then 0.0 else (x - min) / range

    // Step 3: calculate final combined score
    val finalScores = stats.map { s =>
      val visitorsNorm = safeNorm(s.averageVisitors, minVisitors, rangeVisitors)
      val profitNorm = safeNorm(s.averageProfitMargin, minProfit, rangeProfit)

      val combined = (visitorsNorm + profitNorm) / 2

      HotelScore(s.hotelName, combined)
    }

    // Step 4: pick the hotel with the highest score
    Some(finalScores.maxBy(_.score))
}
