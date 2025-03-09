import Context.{ cityMap, scentMap }
import scala.collection._

case class Path(cities: Seq[City]) {
  def addCity(city: City): Path = Path(cities :+ chooseBestCity)

  def distance: Int = (cities zip cities.tail)
    .map{
      case x => cityMap.get(immutable.Set(x._1, x._2)).get
    }.sum

  def chooseBestCity(): City = {
    val bestForCity = mutable.Map[City, (Set[City], Int)]()

    for ((cities, distance) <- cityMap) {
      for (city <- cities) {
        if (!bestForCity.contains(city) || bestForCity(city)._2 > distance) {
          bestForCity(city) = (cities, distance)
        }
      }
    }
    bestForCity.get(cities.last).get._1.head
  }
}
