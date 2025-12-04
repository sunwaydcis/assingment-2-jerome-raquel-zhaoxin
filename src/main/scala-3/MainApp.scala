import scala.util.{Failure, Success}

object MainApp:

  @main def app(): Unit = {
    println("----------Hotel Booking Analysis----------\n")
    CSVLoader.loadFile("/Hotel_Dataset.csv") match
      case Success(bookings) =>
        Question1.run(bookings)
        val question2 = Question2.calculateEconomicScore(bookings)
        println(s"The most economical hotel is ${question2.get.hotelName}")
        val question3 = Question3.calculateHotelPerformance(bookings)
        println(s"The highest performing hotel is ${question3.get.hotelName}")
      case Failure(ex) =>
        println("Failed to load CSV: " + ex.getMessage)
  }

end MainApp