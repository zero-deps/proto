package zero.protopurs

object io {
  def writeToFile(path: String, res: String): Unit = {
    if (res.nonEmpty) {
      import java.io.{BufferedWriter, FileWriter}
      val w = new BufferedWriter(new FileWriter(path))
      w.write(res)
      w.close()
    } else ()
  }
}