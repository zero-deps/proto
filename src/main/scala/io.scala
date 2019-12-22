package zd.proto.purs

object io {
  def writeToFile(path: String, res: String): Unit = {
    import java.io.{BufferedWriter, FileWriter}
    val w = new BufferedWriter(new FileWriter(path))
    w.write(res)
    w.close()
  }
}