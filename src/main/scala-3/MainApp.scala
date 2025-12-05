import scala.util.{Failure, Success}

object MainApp:

  @main def app(): Unit = {
    println("----------Hotel Booking Analysis----------\n")
    CSVLoader.loadFile("/Hotel_Dataset.csv") match
      case Success(bookings) =>
        val question1 = Question1.findTopBookingCountry(bookings)
        println(s"The country with highest number of bookings is ${question1.country} with a total of ${question1.totalBookings} bookings")
        val question2 = Question2.calculateEconomicScore(bookings)
        println(s"The most economical hotel is ${question2.get.hotelName} located in ${question2.get.destinationCity}, ${question2.get.destinationCountry}")
        val question3 = Question3.calculateHotelPerformance(bookings)
        println(s"The highest performing hotel is ${question3.get.hotelName} located in ${question3.get.hotelCity}, ${question3.get.hotelCountry}")
      case Failure(ex) =>
        println("Failed to load CSV: " + ex.getMessage)
  }

end MainApp