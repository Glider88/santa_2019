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
    override def toString: String = "(" + cities.values.mkString(", ") + ")"
  }

  implicit def CityToPoint(city: City): Point = city.point

  sealed abstract class SetTree(val level: Int, val box: Cluster)

  case class SetLeaf(override val level: Int, override val box: Cluster) extends SetTree(level, box) {
    override def toString: String = "cl: " + box.cities.values.map(_.id).mkString(", ")
  }

  case class SetBranch(override val level: Int, override val box: Cluster, row: Set[SetTree]) extends SetTree(level, box) {
    override def toString: String = {
      val head = "cl: " + box.cities.values.map(_.id).mkString(", ") + "\n"
      head + row.map(tree => "    " * tree.level + tree).mkString("\n")
    }
  }

  sealed abstract class ListTree(val level: Int, val start: City, val end: City, val box: Cluster)

  case class ListLeaf(override val level: Int, override val start: City, override val end: City, override val box: Cluster) extends ListTree(level, start, end, box) {
    override def toString: String = "cl: " + box.cities.values.map(_.id).mkString(", ")
  }

  case class ListBranch(override val level: Int, override val start: City, override val end: City, override val box: Cluster, row: List[ListTree]) extends ListTree(level, start, end, box) {
    override def toString: String = {
      val head = "cl: " + box.cities.values.map(_.id).mkString(", ") + "\n"
      head + row.map(tree => "    " * tree.level + tree).mkString("\n")
    }
  }

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

  def closestCity(city: City, clusters: Set[Cluster]): City = {
    val cities = clusters.flatMap(_.cities).toMap
    val startAndEndCity = closestCities(Map(0 -> city), cities)

    startAndEndCity._2
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

    val tmp = tsp(clusters).filter(perms => perms.head == start && perms.last == end)
    if (tmp.isEmpty) {
      throw new RuntimeException(s"catch: start: $start, end: $end, clusters: $clusters")
    }

    tmp.minBy(trackDistance)
  }

  def findByCity(city: City, setOfTree: Set[SetTree]): SetTree = {
    def isDefined(tree: SetTree): Boolean = tree.box.cities.isDefinedAt(city.id)
    setOfTree.find(isDefined).getOrElse(throw new RuntimeException(s"miss: $city in \n$setOfTree"))
  }

  def findClusterCityEars(s: City, e: City, r: List[SetTree]): List[(City, City)] = {
    val closestCityPair = for((c1, c2) <- r.init zip r.tail) yield closestCities(c1.box.cities, c2.box.cities)

    val middle = (List[City]() /: closestCityPair) {
      case (cur, el) => cur ++ List(el._1, el._2)
    }

    val allCities = List(s) ++ middle ++ List(e)

    val ears = allCities.grouped(2).map {
      case List(a, b) => (a, b)
    }.toList

    ears.zipWithIndex.map {
      case ((start, end), index) if start == end && r(index).box.cities.size > 1 => {
        val otherEnd = closestCities(Map(start.id -> start), r(index).box.cities.filterKeys(_ != start.id))
        (start, otherEnd._2)
      }
      case ((start, end), _) => (start, end)
    }
  }
}
