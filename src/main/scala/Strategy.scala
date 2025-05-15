import domain.City
import domain.Context.{cityMap, scentMap}

import scala.util.Random

sealed trait Strategy {
  def chooseBestCity(availableCities: Set[City], lastCity: City, alpha: Int, beta: Int): City
  def weightedChoices(availableCities: Set[City], lastCity: City, alpha: Int, beta: Int): Set[(City, Double)]
}

case object WeightedRandom extends Strategy {
  def chooseBestCity(availableCities: Set[City], lastCity: City, alpha: Int, beta: Int): City = {
    val choices = weightedChoices(availableCities, lastCity, alpha, beta)
    takeRandomAtChance(
      choices.map { case (city, weight) =>
        (city, weight / choices.map(_._2).sum)
      }
    )
  }

  def weightedChoices(availableCities: Set[City], lastCity: City, alpha: Int, beta: Int): Set[(City, Double)] =
    availableCities.flatMap { nextCity =>
      val distance  = cityMap.getOrElse(Set(lastCity, nextCity), Int.MaxValue)
      val pheromone = scentMap.getOrElse(Set(lastCity, nextCity), 1.0)

      if (distance == Int.MaxValue) None
      else {
        val attractiveness =
          math.pow(pheromone, alpha) * math.pow(1.0 / distance, beta)
        Some(nextCity -> attractiveness)
      }
    }

  private def takeRandomAtChance(probabilities: Set[(City, Double)]): City = {
    val r = Random.nextDouble()
    probabilities
      .foldLeft((Option.empty[City], 0.0)) { case ((selected, cumulative), (city, probability)) =>
        val newCumulative = cumulative + probability
        if (selected.isEmpty && newCumulative >= r) (Some(city), newCumulative)
        else (selected, newCumulative)
      }
      ._1
      .getOrElse(probabilities.head._1)
  }
}

case object Naive extends Strategy {
  def chooseBestCity(availableCities: Set[City], lastCity: City, alpha: Int, beta: Int): City =
    weightedChoices(availableCities, lastCity, alpha, beta).maxBy(_._2)._1
  def weightedChoices(availableCities: Set[City], lastCity: City, alpha: Int, beta: Int): Set[(City, Double)] =
    availableCities.flatMap { nextCity =>
      val distance  = cityMap.getOrElse(Set(lastCity, nextCity), Int.MaxValue)
      if (distance == Int.MaxValue) None
      else Some(nextCity -> 1.0 / distance)
    }
}
