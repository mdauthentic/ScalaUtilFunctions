import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor, StandardOpenOption}

object FileIO {

  /**
   * Check if file exist
   * @param fileName: file name
   * @param dir: parent directory
   * */
  def fileExist(fileName: String, dir: String): Boolean = Files.exists(Paths.get(dir, fileName))

  /**
   * Write to file
   *
   * @param dir: file directory
   * @param fileName: file name (append to file if exist or created if it does not exist already)
   * @param data: text to write to file
   *
   * @return Success if file is written or error if otherwise
   * */
  def write2File(dir: String, fileName: String, data: String): Unit = {

    try {
      val path = Paths.get(dir, fileName)
      Files.write(path, data.getBytes("UTF-8"),
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)

      println("Data appended to result file.")

    } catch {

      case e: IOException => e.printStackTrace()
    }
  }

  def readTxtFile(dir: String, fileName: String): Unit = {
    val path = Paths.get(dir, fileName)
    try {
      //Files.lines(path).filter(x => x.startsWith("c")).forEach(println(_))
      Files.lines(path).forEach(println(_))
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  /**
   * Recursively delete file from directory
   * @param dir: directory name or file path
   * */
  def deleteDir(dir: String): Unit = {
    val root = Paths.get(dir)
    Files.walkFileTree(root, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
  }

}
