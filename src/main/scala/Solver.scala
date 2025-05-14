import domain.{ City, Path }
import domain.Context.{ cityMap, scentMap, startingPath }

import scala.annotation.tailrec

class Solver(cities: Set[City], config: Config) {
  private def weightedChoices(availableCities: Set[City], city: City): Set[(City, Double)] =
    availableCities.flatMap { nextCity =>
      val distance  = cityMap.getOrElse(Set(city, nextCity), Int.MaxValue)
      val pheromone = scentMap.getOrElse(Set(city, nextCity), 1.0)

      if (distance == Int.MaxValue) None
      else {
        val attractiveness =
          math.pow(pheromone, config.alpha) * math.pow(1.0 / distance, config.beta)
        Some(nextCity -> attractiveness)
      }
    }

  private def traverseWithAnt(cities: Set[City]): Path = {
    @tailrec
    def oneStepDeeper(currentPath: Path): Path = {
      val canTravelTo = cities.diff(currentPath.cities.toSet)
      if (canTravelTo.isEmpty) currentPath.addCity(currentPath.cities.head)
      else {
        val nextBestCity: City =
          config.strategy.chooseBestCity(weightedChoices(canTravelTo, currentPath.cities.last))
        oneStepDeeper(currentPath.addCity(nextBestCity))
      }
    }
    oneStepDeeper(startingPath)
  }

  def solve: Path =
    (1 to config.iterations).foldLeft(Path(List.empty[City])) { (bestPath, _) =>
      val antPaths    = (1 to config.ants).map(_ => traverseWithAnt(cities)).toList
      val newBestPath = antPaths.minByOption(_.distance).getOrElse(bestPath)
      antPaths.foreach(_.leaveScent())
      scentMap.mapValuesInPlace { (_, value) => value * (1.0 - config.vaporCoeff) }
      newBestPath
    }
}
