package santa.model

import org.scalatest._

class ShellSpec extends FlatSpec with Matchers {
  "Shell" should "calculate angle for two points" in {
    Shell.angle(Point(0.0, 0.0), Point( 1.0,  0.0)) should be (0.0)
    Shell.angle(Point(0.0, 0.0), Point( 1.0,  1.0)) should be (45.0)
    Shell.angle(Point(0.0, 0.0), Point( 0.0,  1.0)) should be (90.0)
    Shell.angle(Point(0.0, 0.0), Point(-1.0,  1.0)) should be (135.0)
    Shell.angle(Point(0.0, 0.0), Point(-1.0,  0.0)) should be (180.0)
    Shell.angle(Point(0.0, 0.0), Point(-1.0, -1.0)) should be (225.0)
    Shell.angle(Point(0.0, 0.0), Point( 0.0, -1.0)) should be (270.0)
    Shell.angle(Point(0.0, 0.0), Point( 1.0, -1.0)) should be (315.0)
  }

  it should "generate angle pairs (min, max)" in {
    Shell.generateAngleSections(180.0) should be (List((0.0, 180.0), (180.0, 360.0)))
    Shell.generateAngleSections(179.0) should be (List((0.0, 179.0), (179.0, 358.0), (358.0, 360.0)))
  }

  it should "map angle sections to city sections" in {
    val center = Point(0.0, 0.0)
    val angleSections = List((0.0, 90.0), (90.0, 180.0), (180.0, 270.0), (270.0, 360.0))
    val cities = Map(
      1 -> City(1, Point( 1.0,  1.0)),
      2 -> City(2, Point( 0.0,  1.0)),
      3 -> City(3, Point(-1.0,  1.0)),
      4 -> City(4, Point(-1.0, -1.0))
    )

    val expected = List(
      Map(1 -> City(1, Point( 1.0,  1.0)), 2 -> City(2, Point( 0.0,  1.0))),
      Map(2 -> City(2, Point( 0.0,  1.0)), 3 -> City(3, Point(-1.0,  1.0))),
      Map(4 -> City(4, Point(-1.0, -1.0))),
      Map.empty
    )

    Shell.toCitySections(center, cities, angleSections) should be (expected)
  }

  it should "map cities sections to far-away city sections" in {
    val center = Point(0.0, 0.0)
    val citySections: List[CityBox] = List(
      Map(1 -> City(1, Point( 1.0, 1.0)), 2 -> City(2, Point(2.0, 1.0))),
      Map(3 -> City(3, Point(-1.0, 1.0))),
      Map.empty
    )
    val cities = Map(
      1 -> City(1, Point( 1.0,  1.0)),
      2 -> City(2, Point( 2.0,  1.0)),
      3 -> City(3, Point(-1.0,  1.0))
    )

    val expected = List(
      List(City(2, Point( 2.0,  1.0))),
      List(City(3, Point(-1.0,  1.0))),
      List.empty
    )

    Shell.toFarCitySections(center, cities, citySections) should be (expected)
  }
}

