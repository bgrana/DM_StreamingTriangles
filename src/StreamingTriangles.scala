import scala.io.Source
import scala.math.pow

class StreamingTriangles(size_edges: Int, size_wedges: Int) {
  
  var edge_res = new Array[(Int, Int)](size_edges)
  var wedge_res = new Array[((Int, Int), (Int, Int))](size_wedges)
  var is_closed = new Array[Boolean](size_wedges)
  var updated_edge_res = false
  var iter = 1
  var wedge_res_empty = true
  
  private def closes(wedge: ((Int, Int), (Int, Int)), edge: (Int, Int)):Boolean = {
    val s1 = Set(wedge._1._1, wedge._1._2)
    val s2 = Set(wedge._2._1, wedge._2._2)
    val s_new = Set(edge._1, edge._2)
    return s1.union(s2).union(s_new).size == 3 && !s1.equals(s_new) && !s2.equals(s_new)
  }
  
  private def update_closed(new_edge: (Int, Int)):Unit = {
    for (i <- 0 to size_wedges-1) {
      if (closes(wedge_res(i), new_edge)) {
        is_closed(i) = true
      }
    }
  }
  
  private def update_wedges_res(new_edge: (Int, Int)):Unit = {
    val(tot_wedges, size_new_wedges) = get_tot_wedges(new_edge)
    val new_wedges = get_new_wedges(new_edge)

    if (tot_wedges != 0) {
      for (i <- 0 to size_wedges-1) {
        val r = scala.util.Random.nextFloat()
        if (r <= size_new_wedges.toDouble/tot_wedges.toDouble) {
          val rand_index = scala.util.Random.nextInt(new_wedges.size)
          wedge_res(i) = new_wedges(rand_index)
          wedge_res_empty = false
          is_closed(i) = false
        }
      }
    }
  }
  
  private def update_edges_res(iter: Int, new_edge: (Int, Int)):Unit = {
    updated_edge_res = false
    for (i <- 0 to size_edges-1) {
      val r = scala.util.Random.nextFloat()
      if (r <= 1/iter.toDouble) {
        edge_res(i) = new_edge
        updated_edge_res = true
      }
    }
  }
  
  private def is_wedge(e1: (Int, Int), e2: (Int, Int)): Boolean = {
    val s1 = Set(e1._1, e1._2)
    val s2 = Set(e2._1, e2._2)
    return s1.intersect(s2).size == 1
  }
  
  private def get_tot_wedges(new_edge: (Int, Int)):(Int, Int) = {
    var all_wedges = 0
    var new_wedges = 0
    for (i <- 0 to size_edges-1) {
      for (j <- i+1 to size_edges-1) {
        if (is_wedge(edge_res(i), edge_res(j))) {
          all_wedges = all_wedges + 1
          if (edge_res(i).equals(new_edge) || edge_res(j).equals(new_edge)) {
            new_wedges = new_wedges + 1
          }
        }
      }
    }
    return (all_wedges, new_wedges)
  }
  
  private def get_new_wedges(new_edge: (Int, Int)): Array[((Int, Int), (Int, Int))] = {
    edge_res.filter(e => is_wedge(e, new_edge))
      .map(x => ((x, new_edge)))
  }

  def getEdgeStream(fpath: String): Stream[(Int, Int)] = {
    // Read file
    val content = Source.fromFile(fpath).mkString
    val edges = content.split("\n")
          .filter(s => s.split(" ").length == 2)
          .map(s => (s.split(" ").apply(0).toInt, s.split(" ").apply(1).toInt))
          .map(t => (t._1.max(t._2), t._1.min(t._2)))
    return edges.toStream
  }
  
  private def update(new_edge: (Int, Int)) = {
    update_edges_res(iter, new_edge)
    
    if (updated_edge_res) {
      update_wedges_res(new_edge)
      if (!wedge_res_empty) {
        update_closed(new_edge)
      }
    }
    iter = iter + 1
  }
  
  def execute(new_edge: (Int, Int)):Unit = {
    update(new_edge)
    val ro = is_closed.filter(_ == true).size.toDouble / is_closed.size.toDouble
    val transitivity = 3 * ro
    val triangles = (ro*pow(iter,2)/(size_edges*(size_edges - 1)))*get_tot_wedges(new_edge)._1
    
    if (iter%10 == 0) {
      println("Iteration " + iter)
      println("Transitivity: " + transitivity)
      println("Triangles: " + triangles)
    }
  }
}