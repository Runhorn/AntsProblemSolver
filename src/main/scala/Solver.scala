import domain.{ City, Path }
import domain.Context.{ scentMap, startingPath }

import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec

class Solver(cities: Set[City], config: Config) {
  private def traverseWithAnt(cities: Set[City]): Path = {
    @tailrec
    def oneStepDeeper(currentPath: Path): Path = {
      val canTravelTo = cities.diff(currentPath.cities.toSet)
      if (canTravelTo.isEmpty) currentPath.addCity(currentPath.cities.head)
      else {
        val cityWeights = config.strategy.weightedChoices(
          canTravelTo,
          currentPath.cities.last,
          config.alpha,
          config.beta
        )
        val nextBestCity: City = config.strategy.chooseBestCity(cityWeights)
        oneStepDeeper(currentPath.addCity(nextBestCity))
      }
    }
    oneStepDeeper(startingPath)
  }

  def solve: Path =
    (1 to config.iterations).foldLeft(Path(List.empty[City])) { (bestPath, _) =>
      def antPaths    = (1 to config.ants).par.map(_ => traverseWithAnt(cities)).toList
      def newBestPath = antPaths.minByOption(_.distance).getOrElse(bestPath)
      config.strategy match {
        case Naive => ()
        case WeightedRandom =>
          antPaths.par.foreach(_.leaveScent())
          scentMap.mapValuesInPlace { (_, value) => value * (1.0 - config.vaporCoeff) }
      }
      newBestPath
    }
}
