package domain

import Context.{ cityMap, scentMap }

case class Path(cities: List[City]) {
  def addCity(city: City): Path = Path(cities :+ city)

  def distance: Int = (cities zip cities.tail).map { case (a, b) =>
    cityMap.getOrElse(Set(a, b), Int.MaxValue)
  }.sum

  def leaveScent(): Unit = {
    for (i <- 0 until cities.length - 1) {
      val route               = Set(cities(i), cities(i + 1))
      val scentAmount: Double = 1.0 / cityMap(route)
      scentMap.updateWith(route) {
        case Some(value) => Some(value + scentAmount)
        case None        => Some(scentAmount)
      }
    }
  }
}
