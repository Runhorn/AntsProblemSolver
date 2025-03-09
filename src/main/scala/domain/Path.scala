package domain

import scala.collection.immutable.{Seq, Set}
import Context.{alpha, beta, cityMap, scentMap}

import scala.util.Random

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

    val totalWeight: Double = weightedChoices.map(_._2).sum
    val probabilities: Seq[(City, Double)] = weightedChoices.map { case (city, weight) => (city, weight / totalWeight) }

    val takeRandomAtChance: City = {
      val r = Random.nextDouble()
      var cumulative = 0.0
      probabilities.find {
        case (_, probability) =>
          cumulative += probability
          cumulative >= r
      }.map(_._1).getOrElse(probabilities.last._1)
    }

    takeRandomAtChance
  }

  def leaveScent: Unit = {
    for (i <- 0 until cities.length - 1) {
      val route = Set(cities(i), cities(i+1))
      val scentAmount: Double = 1.0 / cityMap(route)
      scentMap.updateWith(route) {
        case Some(value) => Some(value + scentAmount)
        case None => Some(scentAmount)
      }
    }
  }
}
