import domain.{ City, Path }
import domain.Context.{ scentMap, startingPoint }

class Solver(cities: Set[City], config: Config) {
  private def traverseWithAnt(cities: Set[City]): Path = {
    def oneStepDeeper(currentPath: Path): Path = {
      val canTravelTo = cities.diff(currentPath.cities.toSet)
      if (canTravelTo.isEmpty) currentPath.addCity(currentPath.cities.head)
      else {
        val nextBestCity: City =
          currentPath.chooseBestCity(canTravelTo, currentPath.cities.last, config.alpha, config.beta)
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
      antPaths.foreach(_.leaveScent)
      scentMap.mapValuesInPlace { (_, value) => value * (1.0 - config.vaporCoeff) }
    }
    bestPath
  }
}
