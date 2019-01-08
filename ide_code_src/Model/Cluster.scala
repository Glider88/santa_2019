package Model

class Cluster(val id: Int, var point: Point) {
  import scala.math._

  def angle(p1: Point, p2: Point) = atan2(p2.y - p1.y, p2.x - p1.x)

  val polar = cities.get(0).getOrElse(throw new RuntimeException("ALARM"))
  val idToAngle = cities mapValues { case city => angle(polar, city) }
  val idToDistance = cities mapValues { case city => distance(polar, city) }

  def toRadiant(angle: Int): Double = angle * Pi / 180
  val stepAngle = 45
  val angles = (0 to 180 by stepAngle map toRadiant) ++ (-179 until 0 by stepAngle map toRadiant)

  def pairs[A](seq: Seq[A]): Seq[(A, A)] = seq.init zip seq.tail

  val matchListCities = pairs(angles.toList).map {
    case (min, max) => cities.filter {
      case (i, city) => {
        val a = idToAngle.get(city.id).getOrElse(throw new RuntimeException("ALARM"))
        a >= min && a <= max
      }
    }
  }

  val shellCities = matchListCities.flatMap(idToCity =>
    if (idToCity.size != 0) {
      implicit object CityOrdering extends Ordering[City] {
        def compare(c1: City, c2: City): Int = {
          val d1 = idToDistance.get(c1.id).getOrElse(throw new RuntimeException("ALARM"))
          val d2 = idToDistance.get(c2.id).getOrElse(throw new RuntimeException("ALARM"))
          d1 compare d2
        }
      }
      List(idToCity.max._2)
    } else {
      List()
    }
  ).toSet
}