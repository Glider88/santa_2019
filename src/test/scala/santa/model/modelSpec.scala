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

  "short track" should "work general case" in {
    val start = City(1, Point(0.0, 0.0))
    val middle1 = City(2, Point(2.0, -1.0))
    val middle2 = City(3, Point(1.0, 1.0))
    val end = City(4, Point(3.0, 0.0))

    val leaf1 = SetLeaf(0, Cluster(start,   Map(1 ->   start), Set(  start)))
    val leaf2 = SetLeaf(0, Cluster(middle1, Map(2 -> middle1), Set(middle1)))
    val leaf3 = SetLeaf(0, Cluster(middle2, Map(3 -> middle2), Set(middle2)))
    val leaf4 = SetLeaf(0, Cluster(end,     Map(4 ->     end), Set(    end)))

    shortTrack(leaf1, leaf4, Set(leaf4, leaf3, leaf2, leaf1)) should be (List(leaf1, leaf3, leaf2, leaf4))
  }

  it should "work for cluster with single city" in {
    val city = City(1, Point(0.0, 0.0))
    val leaf = SetLeaf(0, Cluster(city, Map(1 -> city), Set(city)))

    shortTrack(leaf, leaf, Set(leaf)) should be (List(leaf))
  }

  it should "work when start and end city same" in {
    val point = Point(0.0, 0.0)
    val city1 = City(1, point)
    val city2 = City(2, point)
    val leaf1 = SetLeaf(0, Cluster(point, Map(1 -> city1), Set(city1)))
    val leaf2 = SetLeaf(0, Cluster(point, Map(2 -> city2), Set(city2)))

    shortTrack(leaf1, leaf1, Set(leaf1, leaf2)) should be (List(leaf1, leaf2, leaf1))
  }

  "find by city" should "work" in {
    val p = Point(0.0, 0.0)
    val shell = Set.empty[City]
    val c1 = City(1, p)
    val c2 = City(2, p)
    val c3 = City(3, p)

    val branch = SetBranch(
      0,
      Cluster(p, Map(1 -> c1, 2 -> c2), shell),
      Set(
        SetLeaf(0, Cluster(p, Map(1 -> c1), shell)),
        SetLeaf(0, Cluster(p, Map(2 -> c2), shell))
      )
    )

    val leaf = SetLeaf(0, Cluster(p, Map(3 -> c3), shell))

    findByCity(c1, Set(branch, leaf)) should be (branch)
    findByCity(c2, Set(branch, leaf)) should be (branch)
    findByCity(c3, Set(branch, leaf)) should be (leaf)
  }

  "find cluster ears" should "work for leaf cluster" in {
    val p = Point(0.0, 0.0)
    val c1 = City(1, p)
    val c2 = City(2, p)
    val leaf1 = SetLeaf(0, Cluster(p, Map(1 -> c1), Set(c1)))
    val leaf2 = SetLeaf(0, Cluster(p, Map(2 -> c2), Set(c2)))

    findClusterCityEars(c1, c1, List(leaf1)) should be (List((c1, c1)))
    findClusterCityEars(c1, c2, List(leaf1, leaf2)) should be (List((c1, c1), (c2, c2)))
  }

  "find cluster ears" should "work for branch cluster" in {
    val p = Point(0.0, 0.0)
    val c1 = City(1, Point(0.0,  0.0))
    val c2 = City(2, Point(1.0,  0.0))
    val c3 = City(3, Point(2.0,  1.0))
    val c4 = City(4, Point(2.0, -1.0))
    val c5 = City(5, Point(3.0,  0.0))
    val c6 = City(6, Point(4.0,  0.0))
    val leaf1 = SetLeaf(0, Cluster(p, Map(1 -> c1), Set(c1)))
    val leaf2 = SetLeaf(0, Cluster(p, Map(6 -> c6), Set(c6)))
    val branch = SetBranch(
      0,
      Cluster(p, Map(2 -> c2, 3 -> c3, 4 -> c4, 5 -> c5), Set(c2, c3, c4, c5)),
      Set(
        SetLeaf(0, Cluster(p, Map(2 -> c2), Set(c2))),
        SetLeaf(0, Cluster(p, Map(3 -> c3), Set(c3))),
        SetLeaf(0, Cluster(p, Map(4 -> c4), Set(c4))),
        SetLeaf(0, Cluster(p, Map(5 -> c5), Set(c5)))
      )
    )

    findClusterCityEars(c1, c6, List(leaf1, branch, leaf2)) should be (List((c1, c1), (c2, c5), (c6, c6)))
  }
}