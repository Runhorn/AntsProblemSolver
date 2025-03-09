package domain

import scala.collection.immutable.{Seq, Set}
import Context.{cityMap, scentMap, alpha, beta}

case class Path(cities: Seq[City]) {
  def addCity(city: City): Path = Path(cities :+ city)

  def distance: Int = (cities zip cities.tail)
    .map { case (a,b) => cityMap.getOrElse(Set(a,b), Int.MaxValue) }
    .sum

  def chooseBestCity(city: City): City = {
    val availableCities = cityMap.keys.flatten.filterNot(cities.contains).toSeq

    if (availableCities.isEmpty) return city

    val weightedChoices = availableCities.map { nextCity =>
      val distance = cityMap(Set(city, nextCity))
      val pheromone = scentMap(Set(city, nextCity))
      val attractiveness = math.pow(pheromone, alpha) * math.pow(1.0 / distance, beta)
      (nextCity, attractiveness)
    }

    val totalWeight = weightedChoices.map(_._2).sum
    val probabilities = weightedChoices.map { case (city, weight) => (city, weight / totalWeight) }

    val selectedCity = probabilities.maxBy(_._2)._1
    selectedCity
  }
}
