package santa.algorithm

import java.text.SimpleDateFormat
import java.util.Calendar
import santa.model.{City, CityBox, Cluster}
import vegas._

object Visualization {
  type VegasPoint = Map[String, Double]

  def renderCities(cities: CityBox): String = {
    val cities_viz = cities.values.toList.map(cityToMap(_))

    val html = Vegas("A scatterplot with custom star shapes.").
      withData(cities_viz).
      mark(vegas.Point).
      encodeX("latitude", Quant).
      encodeY("longitude", Quant).
      configCell(width=1280.0, height=1024.0).
      encodeSize(value=1).
      html.
      pageHTML("vegas")

    save(html)
  }

  def runClusters(clusters: Set[Cluster]): Unit = {

    val cities_viz = for {
      cluster <- clusters.toList
      (_, city) <- cluster.cities
    } yield cityToMap(city)

    val teal = "#00ffff"
    val colors = List("#000000", "#0000ff", "#008000", "#800000", "#008080", "#808080", "#ffffff")

    val html = Vegas("A scatterplot with custom star shapes.").
      withData(cities_viz).
      mark(vegas.Point).
      encodeX("latitude", Quant).
      encodeY("longitude", Quant).
      configCell(width=1280.0, height=1024.0).
      encodeColor("groups", Nominal, scale=Scale(rangeNominals=colors)).
      encodeSize(value=30).
      html.
      pageHTML("vegas")

    val filename = save(html)
    println(filename)
  }

  private def cityToMap(city: City, groupId: Int = 1): VegasPoint = {
    Map(
      "id" -> city.id,
      "latitude" -> city.point.x,
      "longitude" -> city.point.y,
      "groups" -> groupId
    )
  }

  private def save(html: String): String = {

    import java.io._

    val now = Calendar.getInstance().getTime
    val format = new SimpleDateFormat("HH_ mm_ss")
    val name = format.format(now)

    val pw = new PrintWriter(new File(s"output/$name.html"))
    pw.write(html)
    pw.close

    s"$name.html"
  }
}
