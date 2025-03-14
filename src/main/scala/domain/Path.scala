package domain

import Context.{ cityMap, scentMap }

import scala.util.Random

case class Path(cities: List[City]) {
  def addCity(city: City): Path = Path(cities :+ city)

  def distance: Int = (cities zip cities.tail).map { case (a, b) =>
    cityMap.getOrElse(Set(a, b), Int.MaxValue)
  }.sum

  def chooseBestCity(city: City, alpha: Int, beta: Int): City = {
    val availableCities = cityMap.keys.flatten.filterNot(cities.contains)

    if (availableCities.isEmpty) return city

    val weightedChoices = availableCities.flatMap { nextCity =>
      val distance  = cityMap.getOrElse(Set(city, nextCity), Int.MaxValue)
      val pheromone = scentMap.getOrElse(Set(city, nextCity), 1.0)

      if (distance == Int.MaxValue) None
      else {
        val attractiveness = math.pow(pheromone, alpha) * math.pow(1.0 / distance, beta)
        Some(nextCity -> attractiveness)
      }
    }.toList

    val totalWeight: Double = weightedChoices.map(_._2).sum
    val probabilities: List[(City, Double)] = weightedChoices.map { case (city, weight) =>
      (city, weight / totalWeight)
    }

    val takeRandomAtChance: City = {
      val r          = Random.nextDouble()
      var cumulative = 0.0
      probabilities
        .find { case (_, probability) =>
          cumulative += probability
          cumulative >= r
        }
        .map(_._1)
        .getOrElse(probabilities.head._1)
    }

    takeRandomAtChance
  }

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
