package santa.algorithm

import santa.model.{CityBox, Cluster, Point, Shell, Vector, distance}

object Clusterization {
  val clusterNumber = 7
  private val random = scala.util.Random

  def randomDoublePairs: List[(Double, Double)] = {
    List.fill(clusterNumber)((random.nextDouble, random.nextDouble))
  }

  def expandPoint(cities: CityBox, pairs: List[(Double, Double)]): List[Point] = {
    val minX = cities.minBy(_._2.point.x)._2.point.x
    val maxX = cities.maxBy(_._2.point.x)._2.point.x

    val minY = cities.minBy(_._2.point.y)._2.point.y
    val maxY = cities.maxBy(_._2.point.y)._2.point.y

    def middle(a: Double, b: Double)(randomDouble: Double): Double = {
      val delta = math.abs(a - b)
      val weightedStep = randomDouble * delta
      math.min(a, b) + weightedStep
    }

    val middleX = middle(minX, maxX)(_)
    val middleY = middle(minY, maxY)(_)

    pairs.map {
      case (x, y) => Point(middleX(x), middleY(y))
    }
  }

  def move(cities: CityBox)(p: Point): Point = {
    val force: Vector = cities.foldLeft(Vector(p, p))( (acc, kv) => {
      val p1 = p
      val p2 = kv._2.point
      val vector = Vector(p1, p2)
      val distCoef = Math.pow(distance(p1, p2), 3)
      val coef = if (distCoef == 0) {
        0.0
      } else {
        3 / distCoef
      }
      acc + vector * coef
    })

    force.end
  }

  def splitCities(cities: CityBox, points: List[Point]): List[CityBox] = {
    val cityIdToPointIndex: Map[Int, Int] = cities.mapValues(
      city => points.zipWithIndex.minBy {
        case (point, _) => distance(city.point, point)
      }._2
    )

    val pointIndexToCityIds: Map[Int, Set[Int]] = cityIdToPointIndex.groupBy { case (_, point) => point}.mapValues(_.keys.toSet)
    val pointIndexToCities: Map[Int, CityBox] = pointIndexToCityIds.mapValues(cities.filterKeys(_))
    points.indices.toList.map(pointIndexToCities.getOrElse(_, Map.empty))
  }

  def run(parent: Cluster): Set[Cluster] = {
    val cityBox = parent.cities
    val mover = move(cityBox)(_)

    var points: List[Point] = expandPoint(cityBox, randomDoublePairs)
    for (_ <- 0 to 100) {
      points = points.map(mover)
    }

    val cities: List[CityBox] = splitCities(cityBox, points)

    val clusters = points zip cities map {
      case (center: Point, cities: CityBox) if cities.nonEmpty => Cluster(center, cities, Shell.generateShell(center, cities))
      case (center: Point, _) => Cluster(center, Map.empty, Set.empty)
    }

    clusters.filter(_.cities.nonEmpty).toSet
  }
}
