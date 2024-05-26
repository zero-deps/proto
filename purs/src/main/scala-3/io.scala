package proto
package purs

object io {
  def writeToFile(path: String, res: String): Unit = {
    if res.nonEmpty then
      import java.io.{BufferedWriter, FileWriter}
      val w = new BufferedWriter(new FileWriter(path))
      w.write(res)
      w.close()
    else ()
  }
}