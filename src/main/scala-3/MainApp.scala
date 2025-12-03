import scala.util.{Failure, Success}

object MainApp:

  @main def app(): Unit = {
    println("----------Hotel Booking Analysis----------\n")
    CSVLoader.loadFile("/Hotel_Dataset.csv") match
      case Success(bookings) =>
        val question1 = Question1.findTopBookingCountry(bookings)
        val question2 = Question2.calculateEconomicScore(bookings)
        println(s"The most economical hotel is ${question2.get.hotelName}")
        val question3result = Question3.calculateMostProfitableHotel(bookings)
        println(question3result)
      case Failure(ex) =>
        println("Failed to load CSV: " + ex.getMessage)
  }

end MainApp