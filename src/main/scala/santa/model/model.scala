package santa

import scala.language.implicitConversions

package object model {
  case class Point(x: Double, y: Double) {
    override def toString: String = s"p($x, $y)"
  }

  case class City(id: Int, point: Point) {
    override def toString: String = s"c#$id(${point.x}, ${point.y})"
  }

  type CityBox = Map[Int, City]
  type Shell = Set[City]

  case class Cluster(center: Point, cities: CityBox, shell: Shell) {
    override def toString: String = "cluster(" + cities.values.mkString(", ") + ")"
  }

  implicit def CityToPoint(city: City): Point = city.point

  sealed abstract class SetTree(val box: Cluster)
  case class SetBranch(override val box: Cluster, row: Set[SetTree]) extends SetTree(box)
  case class SetLeaf(override val box: Cluster) extends SetTree(box) {
    override def toString: String = "leaf(" + box.cities.values.mkString(", ") + ")"
  }

  sealed abstract class ListTree(val start: City, val end: City, val box: Cluster)
  case class ListLeaf(override val start: City, override val end: City, override val box: Cluster) extends ListTree(start, end, box)
  case class ListBranch(override val start: City, override val end: City, override val box: Cluster, row: List[ListTree]) extends ListTree(start, end, box)

  def distance(p1: Point, p2: Point): Double = {
    Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))
  }

  def clusterDistance(c1: Cluster, c2: Cluster): Double = {
    val shell1 = c1.shell.map(_.point)
    val shell2 = c2.shell.map(_.point)
    val (p1, p2) = closestPoints(shell1, shell2)
    distance(p1, p2)
  }

  def closestPoints(points1: Set[Point], points2: Set[Point]): (Point, Point) = {
    val allPairs: Set[(Point, Point)] = for {
      p1 <- points1
      p2 <- points2
    } yield (p1, p2)

    allPairs.minBy {case (p1, p2) => distance(p1, p2) }
  }

  def closestCities(city1: CityBox, city2: CityBox): (City, City) = {
    val allPairs = for {
      c1 <- city1.values
      c2 <- city2.values
    } yield (c1, c2)

    allPairs.minBy {case (p1, p2) => distance(p1.point, p2.point) }
  }

  def primesTo(N: Int): Set[Int] = {
    val isPrime = collection.mutable.BitSet(2 to N: _*) -- (4 to N by 2)
    for (p <- 2 +: (3 to Math.sqrt(N).toInt by 2) if isPrime(p)) {
      isPrime --= p*p to N by p
    }
    isPrime.toSet
  }

  def score(track: List[City]): Double = {
    val trackIds = track.map(_.id).toSet
    val cityIds = (CityRepository.findMinCityId to CityRepository.findMaxCityId).toSet
    if (trackIds != cityIds) {
      val surplus = trackIds -- cityIds
      val missing = cityIds -- trackIds

      throw new RuntimeException(s"Validation error: surplus: $surplus, missing: $missing")
    }

    val distances = for((city1, city2) <- track.init zip track.tail) yield distance(city1, city2)
    val maxCityId = CityRepository.findMaxCityId
    val primes = primesTo(maxCityId)
    def penalty(i: Int, city: City): Double = {
      def isPrime(city: City): Boolean = primes.contains(city.id)
      if (i % 10 == 0 && ! isPrime(city)) 1.1 else 1.0
    }

    val distancesWithPenalty = for (
      (distance, index) <- distances.zipWithIndex
    ) yield distance * penalty(index + 1, CityRepository.find(index))
    distancesWithPenalty.sum
  }

  def tsp[A](elements: Set[A]): List[List[A]] = {
    elements.toList.permutations.map(_.toList).toList
  }

  def shortTrack(start: SetTree, end: SetTree, clusters: Set[SetTree]): List[SetTree] = {
    def trackDistance(track: Seq[SetTree]): Double = {
      val distances = for((cluster1, cluster2) <- track.init zip track.tail) yield clusterDistance(cluster1.box, cluster2.box)
      distances.sum
    }

    tsp(clusters)
      .filter(perms => perms.head == start && perms.last == end)
      .minBy(trackDistance)
  }
}
