import scala.io.Source
import scala.util.Random

object Context {
  def cities: Seq[City] =
    Source.fromFile("src/main/resources/ATT48.txt")
      .getLines()
      .toSeq
      .map(e => e.split(" "))
      .map(x => City(x(0).toInt, x(1).toInt,x(2).toInt))

  def cityMap: Map[Set[City], Int] = cities.combinations(2).map {
    case Seq(cityA: City, cityB: City) => Set(cityA, cityB) -> cityA.distanceTo(cityB)
  }.toMap

  def startingPoint: Seq[City] = Random.shuffle(cities)
}
