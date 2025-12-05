import scala.util.{Failure, Success}

object MainApp:

  @main def app(): Unit = {
    println("----------Hotel Booking Analysis----------\n")
    CSVLoader.loadFile("/Hotel_Dataset.csv") match
      case Success(bookings) =>
        Question1.printAnswer(bookings)
        Question2.printAnswer(bookings)
        Question3.printAnswer(bookings)
      case Failure(ex) =>
        println("Failed to load CSV: " + ex.getMessage)
  }

end MainApp