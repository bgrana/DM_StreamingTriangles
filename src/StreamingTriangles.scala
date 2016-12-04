
class StreamingTriangles(size_edges: Int, size_wedges: Int) {
  
  var edge_res = new Array[(Int, Int)](size_edges)
  var wedge_res = new Array[(Int, Int, Int)](size_wedges)
  var is_closed = new Array[Boolean](size_wedges)
  var updated_edge_res = false
  
  private def closes(wedge: (Int, Int, Int), edge: (Int, Int)):Boolean = {
    return (wedge._1 == edge._1 && wedge._3 == edge._2) || (wedge._1 == edge._2 && wedge._3 == edge._1)
  }
  
  private def update_closed(new_edge: (Int, Int)):Unit = {
    for (i <- 0 to size_wedges) {
      if (closes(wedge_res(i), new_edge)) {
        is_closed(i) = true
      }
    }
  }
  
  private def update_wedges_res(new_edge: (Int, Int)):Unit = {
    val x = scala.util.Random.nextFloat()
    val tot_wedges = get_tot_wedges()
    val new_wedges = get_new_wedges(new_edge)
    val size_new_wedges = new_wedges.length
    
    for (i <- 0 to size_edges) {
      if (x < 1/size_new_wedges/tot_wedges) {
        val rand_index = scala.util.Random.nextInt(size_new_wedges)
        wedge_res(i) = new_wedges(rand_index)
        is_closed(i) = false
      }
    }
  }
  
  private def update_edges_res(iter: Int, new_edge: (Int, Int)):Unit = {
    val r = scala.util.Random.nextFloat()
    updated_edge_res = false

    for (i <- 0 to size_edges) {
      if (r < 1/iter) {
        edge_res(i) = new_edge
        updated_edge_res = true
      }
    }
  }
  
  private def is_wedge(e1: (Int, Int), e2: (Int, Int)): Boolean = {
    val s1 = Set(e1)
    val s2 = Set(e2)
    return s1.intersect(s2).size > 0 && s1.equals(s2)
  }
  
  private def get_tot_wedges():Int = {
    var n_wedges = 0
    for (i <- 0 to size_edges) {
      for (j <- i to size_edges) {
        if (is_wedge(edge_res(i), edge_res(j))) {
          n_wedges = n_wedges + 1
        }
      }
    }
    return n_wedges
  }
  
  private def get_new_wedges(new_edge: Tuple2[Int, Int]):Array[Tuple3[Int, Int, Int]] = {
    return wedge_res // TODO this is only a placeholder value
  }
  
  def execute(new_edge: (Int, Int), iter: Int) = {
    update_closed(new_edge)
    update_edges_res(iter, new_edge)
    if (updated_edge_res) {
      update_wedges_res(new_edge)
    }
  }
}