import domain.{City, Path}
import domain.Context.{cityMap, scentMap, startingPoint}

import scala.annotation.tailrec
import scala.util.Random

class Solver(cities: Set[City], config: Config) {
  private def weightedChoices(availableCities: Set[City], city: City): Set[(City, Double)] =
    availableCities.flatMap { nextCity =>
      val distance  = cityMap.getOrElse(Set(city, nextCity), Int.MaxValue)
      val pheromone = scentMap.getOrElse(Set(city, nextCity), 1.0)

      if (distance == Int.MaxValue) None
      else {
        val attractiveness = math.pow(pheromone, config.alpha) * math.pow(1.0 / distance, config.beta)
        Some(nextCity -> attractiveness)
      }
    }

  private def takeRandomAtChance(probabilities: Set[(City, Double)]): City = {
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

  private def chooseBestCity(choices: Set[(City, Double)]): City = {
    takeRandomAtChance(
      choices.map { case (city, weight) =>
        (city, weight / choices.map(_._2).sum)
      }
    )
  }

  private def traverseWithAnt(cities: Set[City]): Path = {
    @tailrec
    def oneStepDeeper(currentPath: Path): Path = {
      val canTravelTo = cities.diff(currentPath.cities.toSet)
      if (canTravelTo.isEmpty) currentPath.addCity(currentPath.cities.head)
      else {
        val nextBestCity: City =
          chooseBestCity(weightedChoices(canTravelTo, currentPath.cities.last))
        oneStepDeeper(currentPath.addCity(nextBestCity))
      }
    }
    oneStepDeeper(Path(List(startingPoint)))
  }

  def solve: Path = {
    var bestPath: Path = Path(List.empty[City])

    for (i <- 1 to config.iterations) {
      var antPaths: List[Path] = List.empty[Path]
      for (j <- 1 to config.ants) {
        val candidate: Path = traverseWithAnt(cities)
        antPaths = antPaths :+ candidate
        if (bestPath.cities.isEmpty || bestPath.distance > candidate.distance) bestPath = candidate
      }
      antPaths.foreach(_.leaveScent())
      scentMap.mapValuesInPlace { (_, value) => value * (1.0 - config.vaporCoeff) }
    }
    bestPath
  }
}
