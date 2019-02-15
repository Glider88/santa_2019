package santa.algorithm

import santa.{CSV, CityRepository}
import santa.model.City

/*
class NearestNeighbor {
  import scala.collection.mutable

  private var poolCities = mutable.Map[Int, City]() ++= CityRepository.findAll
  val northPole: City = CityRepository.find(0)
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
*/