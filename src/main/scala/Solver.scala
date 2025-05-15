import domain.{ City, Path }
import domain.Context.{ scentMap, startingPath }

import scala.annotation.tailrec

class Solver(cities: Set[City], config: Config) {
  private def traverseWithAnt(cities: Set[City]): Path = {
    @tailrec
    def oneStepDeeper(currentPath: Path): Path = {
      val canTravelTo = cities.diff(currentPath.cities.toSet)
      if (canTravelTo.isEmpty) currentPath.addCity(currentPath.cities.head)
      else {
        val nextBestCity: City =
          config.strategy.chooseBestCity(canTravelTo, currentPath.cities.last, config.alpha, config.beta)
        oneStepDeeper(currentPath.addCity(nextBestCity))
      }
    }
    oneStepDeeper(startingPath)
  }

  def solve: Path =
    (1 to config.iterations).foldLeft(Path(List.empty[City])) { (bestPath, _) =>
      val antPaths    = (1 to config.ants).map(_ => traverseWithAnt(cities)).toList
      val newBestPath = antPaths.minByOption(_.distance).getOrElse(bestPath)
      config.strategy match {
        case Naive => ()
        case WeightedRandom => antPaths.foreach(_.leaveScent())
          scentMap.mapValuesInPlace { (_, value) => value * (1.0 - config.vaporCoeff) }
      }
      newBestPath
    }
}
