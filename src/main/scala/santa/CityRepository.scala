package santa

import santa.model.{City, CityBox}

object CityRepository {
  private val allCities: CityBox = CSV.parse("cities.csv", CSV.parseCity).map(city => city.id -> city).toMap
  private val cities = allCities.filter(el => el._1 != 0 && math.random < 0.005) + (0 -> allCities.getOrElse(0, throw new RuntimeException("Can't find City#0")))

  def find(id: Int): City = {
    cities.getOrElse(id, throw new RuntimeException(s"Can't find City#$id"))
  }

  def findNorthPole: City = find(0)
  def findAll: List[City] = cities.values.toList
  def findAllBox: CityBox = cities
  def findMaxCityId: Int = cities.keys.max
  def findMinCityId: Int = cities.keys.min
}
