package santa.model

object Shell {
  import scala.math.{Pi, atan2}

  def angle(p1: Point, p2: Point): Double = {
    val theta = atan2(p2.y - p1.y, p2.x - p1.x)
    (if (theta >= 0) theta
    else 2 * Pi + theta) * 360 / (2 * Pi)
  }

  def stepAngle(number: Int): Double = {
    if (number < 3) {
      45.0
    } else if(number < 10) {
      35.0
    } else if(number < 20) {
      25.0
    } else if(number < 30) {
      15.0
    } else if(number < 40) {
      5.0
    } else {
      1.0
    }
  }

  def generateAngleSections(step: Double): List[(Double, Double)] = {
    val angles = (0.0 to 360.0 by step).toList
    val lastAngle = 360.0 - angles.last
    val allAngles =
      if (lastAngle > 0.0) {
        angles :+ 360.0
      } else {
        angles
      }

    allAngles.init zip allAngles.tail
  }

  def toCitySections(center: Point, cities: CityBox, angleSections: List[(Double, Double)]): List[CityBox] = {
    val idToAngle = cities.mapValues(city => angle(center, city))
    angleSections.map {
      case (min, max) => cities.filter {
        case (_, city) => {
          val a = idToAngle.getOrElse(city.id, throw new RuntimeException("ALARM"))
          a >= min && a <= max
        }
      }
    }
  }

  def toFarCitySections(center: Point, cities: CityBox, citySections: List[CityBox]): List[List[City]] = {
    val idToDistance = cities.mapValues(city => distance(center, city))
    citySections.map(idToCity =>
      if (idToCity.nonEmpty) {
        implicit object CityOrdering extends Ordering[City] {
          def compare(c1: City, c2: City): Int = {
            val d1 = idToDistance.getOrElse(c1.id, throw new RuntimeException("ALARM"))
            val d2 = idToDistance.getOrElse(c2.id, throw new RuntimeException("ALARM"))
            d1 compare d2
          }
        }
        List(idToCity.max._2)
      } else {
        List()
      }
    )
  }

  def generateShell(center: Point, cities: CityBox): Shell = {
    val step = stepAngle(cities.size)
    val angleSections = generateAngleSections(step)

    val citySections = toCitySections(center, cities, angleSections)
    val farCitySections = toFarCitySections(center, cities, citySections)

    farCitySections.flatten.toSet
  }
}
