object Utils {
  def groupByHotel(bookings: List[HotelBooking]) =
    bookings.groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
  end groupByHotel

  def safeNorm(value: Double, min: Double, max: Double): Double =
    val range = max - min
    if range == 0 then 0.0 else (value - min) / range
  end safeNorm

  /** Generic min/max extractor */
  def minMax[A](items: List[A])(f: A => Double): (Double, Double) =
    val values = items.map(f)
    (values.min, values.max)
  end minMax
}
