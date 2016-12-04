

object Main {
  
  def main(args: Array[String]): Unit = {
    
    val fpath = ""
    val st = new StreamingTriangles(5,5)
    val stream = st.getEdgeStream(fpath)
    stream.foreach(st.execute)
  }
  
}