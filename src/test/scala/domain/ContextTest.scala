package domain

import org.scalatest.funsuite.AnyFunSuite

class ContextTest extends AnyFunSuite {

  val city1 = City(1, 10, 10)
  val city2 = City(2, 20, 20)
  val city3 = City(3, 30, 30)
  val cities: Seq[City] = Seq(city1, city2, city3)

  val cityMap: Map[Set[City], Int] = cities.combinations(2).map {
    case Seq(cityA, cityB) => Set(cityA, cityB) -> cityA.distanceTo(cityB)
  }.toMap

  test("cityMap should be symmetric") {
    for ((pair, dist) <- cityMap) {
      val citiesSeq = pair.toSeq
      assert(cityMap.get(Set(citiesSeq(1), citiesSeq(0))).contains(dist))
    }
  }

  test("cityMap should compute correct distances") {
    val expectedCityMap: Map[Set[City], Int] = Map(
      Set(city1, city2) -> city1.distanceTo(city2),
      Set(city1, city3) -> city1.distanceTo(city3),
      Set(city2, city3) -> city2.distanceTo(city3)
    )

    expectedCityMap.foreach {
      case (pair, expectedDistance) =>
        assert(cityMap.get(pair).contains(expectedDistance), s"Mismatch for $pair")
    }
  }
}
