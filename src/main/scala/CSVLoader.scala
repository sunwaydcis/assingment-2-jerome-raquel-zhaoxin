import MyApp.getClass
import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.util.{Try, Using}

object CSVLoader:
  def loadHotelData(): Try[List[HotelBooking]] =
    val resourcePath = getClass.getResource("/Hotel_Dataset.csv").getPath
    Using(CSVReader.open(new File(resourcePath))) { reader =>
        reader.allWithHeaders().flatMap { row =>
          Try {
            // Parse discount string to double
            val parseDiscountString = row.getOrElse("Discount", "0%").replace("%", "")

            HotelBooking(
              hotelName = row.getOrElse("Hotel Name", "Unknown"),
              originCountry = row.getOrElse("Origin Country", "Unknown"),
              destinationCountry = row.getOrElse("Destination Country", "Unknown"),
              bookingPrice = row.getOrElse("Booking Price[SGD]", "0").toDouble,
              discount = parseDiscountString.toDouble,
              profitMargin = row.getOrElse("Profit Margin", "0").toDouble
            )
          }.toOption
        }
      }
end CSVLoader
