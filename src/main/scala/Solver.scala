import domain.{City, Path}
import domain.Context.{scentMap, startingPath}

import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import com.typesafe.scalalogging.LazyLogging

class Solver(cities: Set[City], config: Config) extends LazyLogging {
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

  def solve: Path = {
    val trace = ArrayBuffer.empty[(Int, Double, Path)]
    val startTime = System.nanoTime

    val result = (1 to config.iterations).foldLeft(Path(List.empty[City])) { (bestPath, iter) =>
      def antPaths    = (1 to config.ants).par.map(_ => traverseWithAnt(cities)).toList
      def newBestPath = antPaths.minByOption(_.distance).getOrElse(bestPath)
      config.strategy match {
        case Naive => ()
        case WeightedRandom =>
          antPaths.par.foreach(_.leaveScent())
          scentMap.mapValuesInPlace { (_, value) => value * (1.0 - config.vaporCoeff) }
      }
      trace += ((iter, newBestPath.distance, newBestPath))
      newBestPath
    }

    val endTime = System.nanoTime
    val execTimeMs = (endTime - startTime) / 1e6

    trace.foreach { case (iter, dist, _) =>
      logger.info(s"Iteration $iter: distance = $dist")
    }
    val bestOverall = trace.minBy(_._2)
    logger.info(s"Best path with distance = ${bestOverall._2} at iteration ${bestOverall._1}")
    logger.info(s"Total execution time: $execTimeMs ms")
    result
  }
}
