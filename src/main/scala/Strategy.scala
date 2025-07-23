import domain.City
import domain.Context.{ cityMap, scentMap }

import scala.util.Random

sealed trait Strategy {
  def chooseBestCity(choices: IndexedSeq[(City, Double)]): City
  def weightedChoices(
      availableCities: Set[City],
      lastCity: City,
      alpha: Int,
      beta: Int
  ): IndexedSeq[(City, Double)]
}

case object WeightedRandom extends Strategy {
  def chooseBestCity(choices: IndexedSeq[(City, Double)]): City = {
    val weights = choices.map(_._2)
    val total = weights.sum
    if (choices.isEmpty) throw new NoSuchElementException("No available cities")
    val normalized = weights.map(_ / total)
    val cumulative = normalized.scanLeft(0.0)(_ + _).tail.toArray
    val r = Random.nextDouble()
    val idx = java.util.Arrays.binarySearch(cumulative, r)
    val pos = if (idx < 0) -idx - 1 else idx
    choices(pos)._1
  }

  def weightedChoices(
      availableCities: Set[City],
      lastCity: City,
      alpha: Int,
      beta: Int
  ): IndexedSeq[(City, Double)] =
    availableCities.toIndexedSeq.flatMap { nextCity =>
      val distance  = cityMap.getOrElse(Set(lastCity, nextCity), Int.MaxValue)
      val pheromone = scentMap.getOrElse(Set(lastCity, nextCity), 1.0)

      if (distance == Int.MaxValue) None
      else {
        val attractiveness =
          math.pow(pheromone, alpha) * math.pow(1.0 / distance, beta)
        Some(nextCity -> attractiveness)
      }
    }
}

case object Naive extends Strategy {
  def chooseBestCity(choices: IndexedSeq[(City, Double)]): City = choices.maxBy(_._2)._1
  def weightedChoices(
      availableCities: Set[City],
      lastCity: City,
      alpha: Int,
      beta: Int
  ): IndexedSeq[(City, Double)] =
    availableCities.toIndexedSeq.flatMap { nextCity =>
      val distance = cityMap.getOrElse(Set(lastCity, nextCity), Int.MaxValue)
      if (distance == Int.MaxValue) None
      else Some(nextCity -> 1.0 / distance)
    }
}
