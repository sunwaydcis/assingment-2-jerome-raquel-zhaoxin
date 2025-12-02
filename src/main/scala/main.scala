import com.github.tototoshi.csv.*
import scala.util.Using
import java.io.File

object MyApp:

  @main def run(): Unit = {
     val hotelData = CSVLoader.loadHotelData()
     print("The most economical hotel is " + calculateEconomicScore(hotelData.get).head.hotelName)
  }

end MyApp