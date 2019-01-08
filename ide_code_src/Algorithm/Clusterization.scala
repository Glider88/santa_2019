package Algorithm

class Clusterization {
  val base = 20

  val minLatitude = cities.minBy(_._2.point.latitude)._2.point.latitude
  val maxLatitude = cities.maxBy(_._2.point.latitude)._2.point.latitude

  val minLongitude = cities.minBy(_._2.point.longitude)._2.point.longitude
  val maxLongitude = cities.maxBy(_._2.point.longitude)._2.point.longitude

  val latitudeDelta = (maxLatitude - minLatitude) / (base + 1)
  val longitudeDelta = (maxLongitude - minLongitude) / (base + 1)

  val clusterPoints = for (
    latitude <- 1 to base;
    longitude <- 1 to base
  ) yield Point(latitude * latitudeDelta, longitude * longitudeDelta)

  var clusters = clusterPoints.zipWithIndex.map {
    case(p: Point, i: Int) => Cluster(i + 1, p)
  }

  def move(cl: Cluster): Cluster = {
    val force = cities.foldLeft(Vector(cl.point, cl.point))( (acc, kv) => {
      val p1 = cl.point
      val p2 = kv._2.point
      val vector = Vector(p1, p2)
      val coef = 3 / Math.pow(distance(p1, p2), 3)
      acc + vector * coef
    })

    Cluster(cl.id, force.point2)
  }

  for (i <- 0 to 100) {
    clusters = clusters.map(move)
    println(clusters.head.point)
  }
}
