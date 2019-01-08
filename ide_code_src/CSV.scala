import Model.{City, Point}

object CSV {
  val path = "../input/"
  def parse[A](filename: String, parseRow: String => Option[A]): List[A] = {
    val bufferedSource = scala.io.Source.fromFile(path + filename)
    val result = bufferedSource.getLines.map(parseRow).flatten.toList
    bufferedSource.close
    result
  }

  def parseCity(line: String): Option[City] = {
    val fields = line.trim.split("""\s*,\s*""")
    try {
      Some(
        City(fields(0).toInt, Point(fields(1).toDouble, fields(2).toDouble))
      )
    } catch {
      case e: Exception => None
    }
  }

  def parseCityId(line: String): Option[Int] = {
    try {
      Some(line.toInt)
    } catch {
      case e: Exception => None
    }
  }

  def saveTrack(filename: String, track: scala.collection.Seq[City]): Unit = {
    val header = "Path"
    import java.io._
    val pw = new PrintWriter(new File(path + filename))
    val content = track.map(city => city.id).mkString("\n")
    pw.write(header + "\n" + content)
    pw.close
  }

//  val cities = CSV.parse("cities.csv", parseCity).map(city => city.id -> city).toMap
//  val primes = primesTo(cities.keys.max)
//  val sample = CSV.parse("sample.csv", parseCityId).map(cityId => cities(cityId))
}
