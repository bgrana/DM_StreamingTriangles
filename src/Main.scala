

object Main {
  
  def main(args: Array[String]): Unit = {
    
    if (args.length < 3) {
      println("Error: Please run the program with the following parameters:")
      println("fPath (String): Path to input file")
      println("Size of edges (Integer): Size of the edge_res array")
      println("Size of wedges (Integer): Size of the wedge_res array")
      System.exit(1)
    }
    
    val fpath = args(0)
    val size_edges = args(1).toInt
    val size_wedges = args(2).toInt

    val st = new StreamingTriangles(size_edges, size_wedges)
    val stream = st.getEdgeStream(fpath)
    stream.foreach(st.execute)
  }
  
}