import Model.{City, Point}

def distance(p1: Point, p2: Point): Double = {
  Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))
}

def primesTo(N: Int) = {
  val isPrime = collection.mutable.BitSet(2 to N: _*) -- (4 to N by 2)
  for (p <- 2 +: (3 to Math.sqrt(N).toInt by 2) if isPrime(p)) {
    isPrime --= p*p to N by p
  }
  isPrime.toSet
}

def score(track: List[City], cities: Map[Int, City]): Double = {
  // ToDo: validation
  val distances = for((city1, city2) <- track.init zip track.tail) yield distance(city1, city2)
  val maxCityId = cities.keys.max
  val primes = primesTo(maxCityId)
  def penalty(i: Int, city: City): Double = {
    def isPrime(city: City): Boolean = primes.contains(city.id)
    if (i % 10 == 0 && ! isPrime(city)) 1.1 else 1.0
  }

  val distancesWithPenalty = for((distance, index) <- distances.zipWithIndex) yield distance * penalty(index + 1, cities(index))
  distancesWithPenalty.sum
}