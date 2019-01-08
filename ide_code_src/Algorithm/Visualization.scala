package Algorithm

class Visualization {
  import $ivy.`org.vegas-viz:vegas_2.11:0.3.11`

  import vegas._
  import vegas.data.External._


  //def findNearCluster(city: City): Cluster = {
  //    clusters.minBy(cl => distance(city.point, cl.point))
  //}

  //val cityIdToCluster = cities.map { case (id, city) => (id, findNearCluster(city)) }

  //def get[A, B](i: A, m: scala.collection.Map[A, B]): B = {m.get(i).getOrElse(throw new RuntimeException("ALARM"))}

  def cityToMap(city: City) = {
    val group = if (city.id == 0) 1 else if(shellCities.contains(city)) 2 else 3
    Map(
      "id" -> city.id,
      "latitude" -> city.point.x,
      "longitude" -> city.point.y,
      //"group" -> get(city.id, cityIdToCluster).id
      "groups" -> group
    )
  }

  val cities_viz = cities.values.map(cityToMap).toList


  val colors = List("#C0C0C0","#808080","#000000","#FF0000","#800000","#FFFF00","#808000","#00FF00","#008000","#00FFFF","#008080","#0000FF","#000080","#FF00FF","#800080")

  Vegas("A scatterplot with custom star shapes.").
    withData(cities_viz).
    mark(vegas.Point).
    encodeX("latitude", Quant).
    encodeY("longitude", Quant).
    configCell(width=800, height=600).
    encodeColor("groups", Nominal, scale=Scale(rangeNominals=colors)).
    encodeSize(value=1).
    show

}
