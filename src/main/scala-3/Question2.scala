object Question2 {

  case class EconomicalScore(
                              hotelName: String,
                              averagePrice: Double,
                            averageDiscount: Double,
                              averageProfitMargin: Double
                            )

  case class EconomicScorePercentage(
                                      hotelName: String,
                                      score: Double
                                    )

  def calculateEconomicScore(bookings: List[HotelBooking]): Option[EconomicScorePercentage] =
    if bookings.isEmpty then return None

    val scores =
      bookings.groupBy(_.hotelName).map { case (hotelName, bs) =>
        val effective = bs.map(b => b.bookingPrice / b.rooms / b.noOfDays)

        EconomicalScore(
          hotelName,
          averagePrice = effective.sum / effective.length,
          averageDiscount = bs.map(_.discount).sum / bs.length,
          averageProfitMargin = bs.map(_.profitMargin).sum / bs.length
        )
      }.toList

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

      // invert price so cheaper → better score
      val combined = (1 - priceNorm + discountNorm + 1- profitNorm) / 3

      EconomicScorePercentage(s.hotelName, combined)
    }

    Some(finals.maxBy(_.score))

//
//  def calculateEconomicScore(bookings: List[HotelBooking]): Option[EconomicScorePercentage] =
//    if bookings.isEmpty then return None
//
//    val minPriceOfAllBookings =
//      bookings.map(b => b.bookingPrice / b.rooms / b.noOfDays).min
//
//    val maxPriceOfAllBookings =
//      bookings.map(b => b.bookingPrice / b.rooms / b.noOfDays).max
//
//    val grouped = bookings.groupBy(_.hotelName)
//
//    val scores: List[EconomicalScore] = grouped.map { case (hotelName, hotelBookings) =>
//      val effectivePrices =
//        hotelBookings.map(b => b.bookingPrice / b.rooms / b.noOfDays)
//
//      val avgPrice = effectivePrices.sum / effectivePrices.length
//
//      val avgDiscount = hotelBookings.map(_.discount).sum / hotelBookings.length
//      val avgProfitMargin = hotelBookings.map(_.profitMargin).sum / hotelBookings.length
//
//      EconomicalScore(hotelName, avgPrice, avgDiscount, avgProfitMargin)
//    }.toList
//
//    val minAveragePrice = scores.map(_.averagePrice).min
//    val maxAveragePrice = scores.map(_.averagePrice).max
//    val range = maxAveragePrice - minAveragePrice
//
//    val minAverageDiscount = scores.map(_.averageDiscount).min
//    val maxAverageDiscount = scores.map(_.averageDiscount).max
//    val rangeDiscount = maxAverageDiscount - minAverageDiscount
//
//    val minAverageProfitMargin = scores.map(_.averageProfitMargin).min
//    val maxAverageProfitMargin = scores.map(_.averageProfitMargin).max
//    val rangeProfitMargin = maxAverageProfitMargin - minAverageProfitMargin
//
//    // avoid division by zero if all hotels have identical average prices
//    if range == 0 then
//      return Some(EconomicScorePercentage(scores.head.hotelName, 0.0))
//
//    val finalScores = scores.map { s =>
//      val averagePriceScore = (s.averagePrice - minAveragePrice) / range
//      val averageDiscountScore = (s.averageDiscount - minAverageDiscount) / rangeDiscount
//      val averageProfitMarginScore = (s.averageProfitMargin - minAverageProfitMargin) / rangeProfitMargin
//
//      val score = (averagePriceScore + averageDiscountScore + averageProfitMarginScore) / 3
//
//      EconomicScorePercentage(s.hotelName, score)
//    }
//
//    // Most economical = smallest price percentage
//
//    Some(finalScores.minBy(_.score))
//  end calculateEconomicScore
}

//import scala.math.Fractional.Implicits.infixFractionalOps
//import scala.math.Integral.Implicits.infixIntegralOps
//import scala.math.Numeric.Implicits.infixNumericOps
//
//object Question2 {
//  case class EconomicalScore(
//    hotelName: String,
//    averagePrice: Double
//  )
//
//  case class EconomicScorePercentage(
//                                    hoteName: String
//                                    score: Double
//                                    )
//
//  def calculateEconomicScore(bookings: List[HotelBooking]): Option[EconomicalScore] =
//    val minPriceOfAllBookings = bookings.map(b => b.bookingPrice / b.rooms / b.noOfDays).min
//    val maxPriceOfAllBookings = bookings.map(b => b.bookingPrice / b.rooms / b.noOfDays).max
//
//    val grouped = bookings.groupBy(_.hotelName)
//    val scores = grouped.map { case (hotelName, hotelBookings) =>
//      // Normalise price: price per room per day
//      val effectivePrices = hotelBookings.map { b =>
//        b.bookingPrice / b.rooms / b.noOfDays
//      }
//      val avgPrice = effectivePrices.sum / effectivePrices.length
//
//      val maxminP = 1- (avgPrice / )
//
//      val discounts = hotelBookings.map(_.discount)
//      val minD = discounts.min
//      val maxD = discounts.max
//      val maxminD = maxD - minD
//
//      val profitMargins = hotelBookings.map(_.profitMargin)
//      val minPM = profitMargins.min
//      val maxPM = profitMargins.max
//      val maxminPM = maxPM - minPM
//
//      val score = (-maxminP + maxminD + maxminPM) / 3
//
////      val avgDiscount = hotelBookings.map(_.discount).sum / hotelBookings.length
////      val avgProfitMargin = hotelBookings.map(_.profitMargin).sum / hotelBookings.length
//
//      EconomicalScore(
//        hotelName,
//        avgPrice
//      )
//    }
//
//    // Min/Max of averagePrice across all scores
//    val minAveragePrice = scores.map(_.averagePrice).min
//    val maxAveragePrice = scores.map(_.averagePrice).max
//
//
//    val finalResult = scores.map { case (hotelName, avgPrice) =>
//      val averagePercentage = (avgPrice - minAveragePrice) / (maxAveragePrice - minAveragePrice)
//
//      EconomicScorePercentage(
//        hotelName,
//        averagePercentage
//      )
//    }
//
//
//    // Most economical = smallest price variation
//    Some(EconomicScorePercentage.maxBy(_.averagePercentage))
//  end calculateEconomicScore
//}
