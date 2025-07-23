package domain

import Context.{ cityMap, scentMap }

case class Path(cities: List[City]) {
  def addCity(city: City): Path = Path(cities :+ city)

  def distance: Int = (cities zip cities.tail).map { case (a, b) =>
    cityMap.getOrElse(Set(a, b), Int.MaxValue)
  }.sum

  def leaveScent(): Unit = {
    val pheromone = 1.0 / (this.distance + 1e-6)
    cities.sliding(2).foreach {
      case List(a, b) =>
        val edge = Set(a, b)
        scentMap.synchronized {
          scentMap.updateWith(edge) {
            case Some(value) => Some(value + pheromone)
            case None        => Some(pheromone)
          }
        }
      case _ => ()
    }
  }
}
