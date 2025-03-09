import domain.{City, Path}
import domain.Context.{startingPoint, cities}
import utils.Image

object AntAlgorithmSolver extends App {
  private val ants: Int = 1
  private val iterations: Int = 1000

  private def traverseWithAnt(cities: Set[City]): Path = {
    def oneStepDeeper(currentPath: Path): Path = {
      val canTravelTo = cities.diff(currentPath.cities.toSet)
      if(canTravelTo.isEmpty) currentPath
      else {
        val nextBestCity: City = currentPath.chooseBestCity(currentPath.cities.last)
        oneStepDeeper(currentPath.addCity(nextBestCity))
      }
    }
    oneStepDeeper(Path(Seq(startingPoint)))
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
  Image.draw(bestPath)
}

