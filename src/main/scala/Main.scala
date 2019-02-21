import java.text.SimpleDateFormat
import java.util.Calendar

import santa.CityRepository
import santa.algorithm.{Clusterization, Visualization}
import santa.model.{closestCity, clusterDistance, City, Cluster, ListBranch, ListLeaf, ListTree, Point, SetBranch, SetLeaf, SetTree, Shell, findByCity, findClusterCityEars, score, shortTrack, closestPoints, closestCities}
import santa.CSV.saveTrack

import scala.annotation.tailrec

object Main extends App {
  val polar = CityRepository.findNorthPole
  val cities = CityRepository.findAllBox
  val rootCluster = Cluster(polar, cities, Shell.generateShell(polar, cities))

  println("-----------------------------------------------")
  println(Visualization.renderCities(cities))

  def clusterization(level: Int, root: Cluster): SetTree = {
    val clusters = Clusterization.run(root)
    if (clusters.size == 1) {
      SetLeaf(level, box = root)
    } else {
      SetBranch(level, box = root, row = for {cluster <- clusters} yield clusterization(level + 1, cluster))
    }
  }

  val setTree = clusterization(0, rootCluster)
  println("-----------------------------------------------")
  println(setTree)

  def tracking(start: City, end: City, original: SetTree): ListTree = original match {
    case original: SetLeaf => ListLeaf(original.level, start, end, original.box)
    case original: SetBranch => {
      val startCluster = findByCity(start, original.row)
      val endCluster = findByCity(end, original.row)
      val track = shortTrack(startCluster, endCluster, original.row)
      val ears = findClusterCityEars(start, end, track)

      val clusters = (track zip ears).map {
        case (tt: SetTree, (cc1: City, cc2: City)) => tracking(cc1, cc2, tt)
      }

      ListBranch(original.level, start, end, original.box, clusters)
    }
  }

  val firstLevelBranch = setTree match {
    case _: SetLeaf => throw new RuntimeException("need more cities")
    case t: SetBranch => t.row
  }

  val firstLevelClusters: Set[Cluster] = firstLevelBranch.map(_.box)

  val polarCluster = firstLevelClusters.find(_.cities.contains(0)).getOrElse(throw new RuntimeException("cannot find polar cluster"))
  val otherClusters = firstLevelClusters - polarCluster
  val endCity = closestCity(polar, otherClusters)

  println("-----------------------------------------------")
  println(endCity)

  val listTree = tracking(polar, endCity, setTree)
  println("-----------------------------------------------")
  println(listTree)

  def citiesTrack(track: List[City], tree: ListTree): List[City] = tree match {
    case tree: ListLeaf => tree.start :: track
    case tree: ListBranch => track ++ tree.row.flatMap(citiesTrack(track, _))
  }

  val resultCities = citiesTrack(List(), listTree)
  println("-----------------------------------------------")
  println(resultCities)

  // println("-----------------------------------------------")
  // val result = score(resultCities)
  // println(result)

  import java.io._

  val now = Calendar.getInstance().getTime
  val format = new SimpleDateFormat("HH_ mm_ss")
  val name = format.format(now)

  saveTrack(s"$name.csv", resultCities)
}
