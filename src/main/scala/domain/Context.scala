package domain

import scala.collection.mutable
import scala.collection.immutable.{Map, Seq, Set}
import scala.io.Source
import scala.util.Random

object Context {
  val alpha: Int = 1
  val beta: Int = 5

  val cities: Seq[City] =
    Source.fromFile("src/main/resources/ATT48.txt")
      .getLines()
      .toSeq
      .map(e => e.trim.split("\\s+"))
      .map(x => City(x(0).toInt, x(1).toInt,x(2).toInt))

  val cityMap: Map[Set[City], Int] = cities.combinations(2).map {
    case Seq(cityA: City, cityB: City) => Set(cityA, cityB) -> cityA.distanceTo(cityB)
  }.toMap

  val initialScentMap: Map[Set[City], Double] = cities.combinations(2).map {
    case Seq(cityA: City, cityB: City) => Set(cityA, cityB) -> 1.0
  }.toMap

  val scentMap: mutable.Map[Set[City], Double] = collection.mutable.Map(initialScentMap.toSeq: _*)

  def startingPoint: City = Random.shuffle(cities).head
}
