import santa.CityRepository
import santa.algorithm.{Clusterization, Visualization}
import santa.model.{City, Cluster, ListBranch, ListLeaf, ListTree, SetBranch, SetLeaf, SetTree, Shell, findClusterCityEars, findByCity, shortTrack}

import scala.annotation.tailrec

object Main extends App {
  val cities = CityRepository.findAllBox
  val polar = CityRepository.findNorthPole
  val rootCluster = Cluster(polar, cities, Shell.generateShell(polar, cities))

  def clusterization(level: Int, root: Cluster): SetTree = {
    val clusters = Clusterization.run(root)
    if (clusters.size == 1) {
      SetLeaf(level, box = root)
    } else {
      SetBranch(level, box = root, row = for {cluster <- clusters} yield clusterization(level + 1, cluster))
    }
  }

  val setTree = clusterization(0, rootCluster)
  println(setTree)

  def tracking(start: City, end: City, original: SetTree): ListTree = original match {
    case original: SetLeaf => ListLeaf(start, end, original.box)
    case original: SetBranch => {
      val startCluster = findByCity(start, original.row)
      val endCluster = findByCity(end, original.row)
      val track = shortTrack(startCluster, endCluster, original.row)
      val ears = findClusterCityEars(start, end, track)

      val clusters = (track zip ears).map {
        case (tt: SetTree, (cc1: City, cc2: City)) => tracking(cc1, cc2, tt)
      }

      ListBranch(start, end, original.box, clusters)
    }
  }

  val listTree = tracking(polar, polar, setTree)
  println(listTree)
}
