package domain

import scala.collection.mutable
import scala.collection.immutable.{ Map, Seq, Set }
import scala.io.Source
import scala.util.Random

object Context {
  val cities: List[City] =
    Source
      .fromFile("ATT48.txt")
      .getLines()
      .toList
      .map(e => e.trim.split("\\s+"))
      .map(x => City(x(0).toInt, x(1).toInt, x(2).toInt))

  val cityMap: Map[Set[City], Int] = cities
    .combinations(2)
    .map { case List(cityA: City, cityB: City) =>
      Set(cityA, cityB) -> cityA.distanceTo(cityB)
    }
    .toMap

  val scentMap: mutable.Map[Set[City], Double] = collection.mutable.Map(
    cities
      .combinations(2)
      .map { case List(cityA: City, cityB: City) =>
        Set(cityA, cityB) -> 1.0
      }
      .toMap
      .toList: _*
  )

  def startingPoint: City = Random.shuffle(cities).head
}
