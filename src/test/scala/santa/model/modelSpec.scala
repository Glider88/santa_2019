package santa.model

import org.scalatest._

class modelSpec extends FlatSpec with Matchers {
  "distance" should "calculate between two points" in {
    distance(Point(1.0, 1.0), Point(2.0, 2.0)) should be (Math.sqrt(2))
    distance(Point(0.0, 0.0), Point(0.0, 0.0)) should be (0.0)
  }

  it should "calculate between two cities" in {
    distance(City(1, Point(1.0, 1.0)), City(2, Point(2.0, 2.0))) should be (Math.sqrt(2))
    distance(City(3, Point(0.0, 0.0)), City(4, Point(0.0, 0.0))) should be (0.0)
  }

  def city0 = City(0, Point(0.0, 0.0))
  def city1 = City(1, Point(1.0, 1.0))

  def citiesDataProvider: List[(CityBox, CityBox, (City, City))] = {
    val cityBox1 = Map(
      2 -> City(2, Point(1.0, 2.0)),
      3 -> City(3, Point(0.0, 1.0)),
      4 -> City(4, Point(1.0, 0.0))
    )

    val cityBox2 = Map(
      5 -> City(5, Point(-2.0, 0.0)),
      6 -> City(6, Point(-1.0, 1.0)),
      7 -> City(7, Point(-2.0, 2.0))
    )

    List(
      (cityBox1, cityBox2, (City(3, Point(0.0, 1.0)), City(6, Point(-1.0, 1.0))))
    )
  }

  "closest" should "calculate between single points" in {
    closestPoints(Set(city0.point), Set(city0.point)) should be ((city0.point, city0.point))
  }

  it should "calculate between single cities" in {
    closestCities(Map(0 -> city0), Map(0 -> city0)) should be ((city0, city0))
  }

  it should "calculate between collision points" in {
    val result = closestPoints(Set(city0.point, city1.point), Set(city1.point, city0.point))
    val expected1 = (city0.point, city0.point)
    val expected2 = (city1.point, city1.point)

    result should (be (expected1) or be (expected2))
  }

  it should "calculate between collision cities" in {
    val result = closestCities(Map(0 -> city0, 1 -> city1), Map(0 -> city0, 1 -> city1))
    val expected1 = (city0, city0)
    val expected2 = (city1, city1)

    result should (be (expected1) or be (expected2))
  }

  it should "calculate between cities" in {
    for ((cityBox1, cityBox2, expected) <- citiesDataProvider) {
      closestCities(cityBox1, cityBox2) should be (expected)
    }
  }

  it should "calculate between points" in {
    for ((cityBox1, cityBox2, expected) <- citiesDataProvider) {
      val pointsSet1 = cityBox1.mapValues(_.point).values.toSet
      val pointsSet2 = cityBox2.mapValues(_.point).values.toSet
      closestPoints(pointsSet1, pointsSet2) should be ((expected._1.point, expected._2.point))
    }
  }

  "tsp" should "work for set with single element" in {
    tsp(Set(1)) should be (List(List(1)))
  }

  it should "work for set with duplicates" in {
    tsp(Set(1, 1)) should be (List(List(1)))
  }

  it should "work general case" in {
    tsp(Set(1, 2, 3)) should be (List(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1)))
  }
}