//import MyApp.getClass
import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.util.{Try, Using}

object CSVLoader:

  def loadFile(fileName: String): Try[List[HotelBooking]] =
    val resourcePath = getClass.getResource(fileName).getPath

    Using(CSVReader.open(new File(resourcePath))) { reader =>
      reader.allWithHeaders().flatMap { row =>
        Try {
          // Parse numeric values safely
          val discountStr = row.getOrElse("Discount", "0%").replace("%", "")
          HotelBooking(
            bookingID = row.getOrElse("Booking ID", "Unknown"),
            dateOfBooking = row.getOrElse("Date of Booking", ""),
            time = row.getOrElse("Time", ""),
            customerID = row.getOrElse("Customer ID", ""),
            gender = row.getOrElse("Gender", ""),
            age = row.getOrElse("Age", "0").toInt,
            originCountry = row.getOrElse("Origin Country", ""),
            state = row.getOrElse("State", ""),
            location = row.getOrElse("Location", ""),
            destinationCountry = row.getOrElse("Destination Country", ""),
            destinationCity = row.getOrElse("Destination City", ""),
            noOfVisitors = row.getOrElse("No. Of People", "0").toInt,
            checkInDate = row.getOrElse("Check-in date", ""),
            noOfDays = row.getOrElse("No of Days", "0").toInt,
            checkOutDate = row.getOrElse("Check-Out Date", ""),
            rooms = row.getOrElse("Rooms", "0").toInt,
            hotelName = row.getOrElse("Hotel Name", ""),
            hotelRating = row.getOrElse("Hotel Rating", "0").toDouble,
            paymentMode = row.getOrElse("Payment Mode", ""),
            bankName = row.getOrElse("Bank Name", ""),
            bookingPrice = row.getOrElse("Booking Price[SGD]", "0").toDouble,
            discount = discountStr.toDouble,
            gst = row.getOrElse("GST", "0").toDouble,
            profitMargin = row.getOrElse("Profit Margin", "0").toDouble
          )
        }.toOption
      }
    }
  end loadFile
end CSVLoader
