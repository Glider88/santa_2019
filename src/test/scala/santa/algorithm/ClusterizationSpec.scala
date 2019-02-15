package santa.algorithm

import org.scalatest._
import santa.model.{City, Point}

class ClusterizationSpec extends FlatSpec with Matchers {
  "Clusterization" should "generate random initial points" in {
    val pairs = List((0.1, 0.2), (0.3, 0.4), (0.5, 0.6))

    val cities = Map(
      1 -> City(1, Point(1.0, 2.0)),
      2 -> City(2, Point(2.0, 3.0)),
      3 -> City(3, Point(3.0, 2.0)),
      4 -> City(4, Point(2.0, 1.0))
    )

    val points = Clusterization.expandPoint(cities, pairs)
    val expected = List(Point(1.2, 1.4), Point(1.6, 1.8), Point(2.0, 2.2))

    points should be (expected)
  }

  it should "move point to city" in {
    val cities = Map(1 -> City(1, Point(2.0, 0.0)))
    val initPoint = Point(0.0, 0.0)

    val resultPoint = Clusterization.move(cities)(initPoint)
    val expectedPoint = Point((3.0 * 2.0)/math.pow(2.0, 3), 0.0)

    expectedPoint should be (resultPoint)
  }

  it should "not moving in stable position" in {
    val cities = Map(
      1 -> City(1, Point(1.0, 2.0)),
      2 -> City(2, Point(2.0, 3.0)),
      3 -> City(3, Point(3.0, 2.0)),
      4 -> City(4, Point(2.0, 1.0))
    )

    val initPoint = Point(2.0, 2.0)

    val resultPoint = Clusterization.move(cities)(initPoint)

    initPoint should be (resultPoint)
  }

  it should "not moving if point and city have same position" in {
    val cities = Map(1 -> City(1, Point(0.0, 0.0)))
    val initPoint = Point(0.0, 0.0)

    val resultPoint = Clusterization.move(cities)(initPoint)

    initPoint should be (resultPoint)
  }

  it should "split two city for each point" in {
    val cities = Map(
      1 -> City(1, Point(2.0, 0.0)),
      2 -> City(2, Point(1.0, 2.0))
    )

    val points = List(
      Point(0.0, 1.0),
      Point(3.0, 1.0)
    )

    val expected = List(
      Map(2 -> City(2, Point(1.0, 2.0))),
      Map(1 -> City(1, Point(2.0, 0.0)))
    )

    val result = Clusterization.splitCities(cities, points)

    result should be (expected)
  }

  it should "split two city on equals distance" in {
    val cities = Map(
      1 -> City(1, Point(1.0, 0.0)),
      2 -> City(2, Point(1.0, 2.0))
    )

    val points = List(
      Point(0.0, 1.0),
      Point(2.0, 1.0)
    )

    val expected = List(
      Map(
        1 -> City(1, Point(1.0, 0.0)),
        2 -> City(2, Point(1.0, 2.0))
      ),
      Map.empty
    )

    val result = Clusterization.splitCities(cities, points)

    result should be (expected)
  }

  it should "split two city for only one point" in {
    val cities = Map(
      1 -> City(1, Point(0.0, 0.0)),
      2 -> City(2, Point(0.0, 2.0))
    )

    val points = List(
      Point(1.0, 1.0),
      Point(0.0, 1.0)
    )

    val expected = List(
      Map.empty,
      Map(
        1 -> City(1, Point(0.0, 0.0)),
        2 -> City(2, Point(0.0, 2.0))
      )
    )

    val result = Clusterization.splitCities(cities, points)

    result should be (expected)
  }
}
