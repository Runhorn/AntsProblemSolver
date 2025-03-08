import Context.{ cities, startingPoint }

object AntAlgorithmSolver extends App {
  private val ants: Int = 1
  private val alpha: Int = 1
  private val beta: Int = 5
  private val iterations: Int = 10

  private def traverseWithAnt(cities: Set[City]): Path = {
    def oneStepDeeper(currentPath: Path): Path = {
      val canTravelTo = cities.filterNot(currentPath.cities.contains)
      if(canTravelTo.isEmpty) currentPath
      else {
        canTravelTo
          .map(currentPath.addCity)
          .map(oneStepDeeper)
          .minBy(_.distance)
      }
    }
    oneStepDeeper(Path(startingPoint))
  }

  var bestPath: Path = Path(Seq.empty[City])
  for (i <- 1 to iterations) {
    val candidate: Path = traverseWithAnt(cities.toSet)
    if(bestPath.cities.isEmpty || bestPath.distance > candidate.distance) {
      bestPath = candidate
      println(s"New record: ${candidate.distance} at iteration $i")
    }
  }
  println("Found best path:")
  println(bestPath.cities.map(_.id).mkString("->"))
  println(s"with distance: ${bestPath.distance}.")
  println(s"Completed the program.")
}

