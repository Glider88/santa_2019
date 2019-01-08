package Algorithm

class NearestNeighbor {
  import scala.collection.mutable

  def get[A, B](i: A, m: scala.collection.Map[A, B]): B = {m.get(i).getOrElse(throw new RuntimeException("ALARM"))}

  var poolCities = mutable.Map[Int, City]() ++= cities
  val northPole: City = get(0, poolCities)
  var track: mutable.LinkedList[City] = mutable.LinkedList(northPole)
  var currentCity = northPole

  var i = 0
  while (poolCities.size > 1) {
    poolCities -= currentCity.id
    var idToDistance = poolCities mapValues {case city => distance(currentCity, city)}
    var nextCity = get(idToDistance.minBy(_._2)._1, poolCities)
    track = track :+ nextCity
    currentCity = nextCity
    if (i % 1000 == 0) {
      println(poolCities.size)
    }
    i += 1
  }

  track = track :+ northPole

  val newTrack = track.map(c => City(c.id, c.latitude, c.longitude)).toList

  CSV.saveTrack("NN.csv", newTrack)
}
