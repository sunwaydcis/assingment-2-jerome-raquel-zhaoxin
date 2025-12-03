

object MyApp:

  @main def run(): Unit = {
     val hotelData = CSVLoader.loadHotelData()
     print("The most economical hotel is " + calculateEconomicScore(hotelData.get).head.hotelName)
  }

end MyApp