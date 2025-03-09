import org.scalatest.funsuite.AnyFunSuite

class CityTest extends AnyFunSuite {
  test("distanceTo should return 0 when comparing the same city") {
    val city = City(1, 100, 100)
    assert(city.distanceTo(city) == 0)
  }

  test("distanceTo should return correct distance for known points") {
    val city1 = City(1, 0, 0)
    val city2 = City(2, 10, 0)
    val city3 = City(3, 0, 10)
    val city4 = City(4, 6, 8)

    assert(city1.distanceTo(city2) == 3)
    assert(city1.distanceTo(city3) == 3)
    assert(city1.distanceTo(city4) == 3)
  }

  test("distanceTo should be symmetric") {
    val cityA = City(1, 30, 40)
    val cityB = City(2, 60, 80)

    assert(cityA.distanceTo(cityB) == cityB.distanceTo(cityA))
  }
}
