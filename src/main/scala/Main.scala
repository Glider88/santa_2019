import santa.CityRepository
import santa.algorithm.{Clusterization, Visualization}
import santa.model.{City, Cluster, ListBranch, ListLeaf, ListTree, SetBranch, SetLeaf, SetTree, Shell, closestCities, clusterDistance, shortTrack}

import scala.annotation.tailrec

object Main extends App {
  val cities = CityRepository.findAllBox
  val polar = CityRepository.findNorthPole
  val rootCluster = Cluster(polar, cities, Shell.generateShell(polar, cities))

  def clusterization(root: Cluster): SetTree = {
    val clusters = Clusterization.run(root)
    if (clusters.size == 1) {
      SetLeaf(box = root)
    } else {
      SetBranch(box = root, row = for {cluster <- clusters} yield clusterization(cluster))
    }
  }

  val setTree = clusterization(rootCluster)
  println(setTree)

  def tracking(start: City, end: City, original: SetTree): ListTree = original match {
    case original: SetLeaf => ListLeaf(start, end, original.box)
    case original: SetBranch => {
      def findByCity(city: City, setOfTree: Set[SetTree]): SetTree = {
        def isDefined(tree: SetTree): Boolean = tree.box.cities.isDefinedAt(city.id)
        setOfTree.find(isDefined).getOrElse(throw new RuntimeException("miss")) // break here
      }

      def findClusterCityEars(s: City, e: City, r: List[SetTree]): List[(City, City)] = {
        val closestCityPair = for((c1, c2) <- r.init zip r.tail) yield closestCities(c1.box.cities, c2.box.cities)

        val middle = (List[City]() /: closestCityPair) {
          case (cur, el) => cur ++ List(el._1, el._2)
        }

        val allCities = List(s) ++ middle ++ List(e)

        allCities.init zip allCities.tail
      }

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
