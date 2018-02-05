/*
 * Copyright (C) 2017 Lightbend Inc. <http://www.lightbenc.com>
 */
// Copy-pasted from https://github.com/scala/jardiff/blob/14956376d8a55679fddad63e362089c0edb7bdd9/core/src/main/scala/scala/tools/jardiff/IOUtil.scala
// BSD-3-Clause licence
// https://github.com/scala/jardiff/blob/master/LICENSE
package scala.meta.cli.metacp

import java.io.IOException
import java.net.URI
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.SimpleFileVisitor
import java.{util => ju}

object IOUtil {
  def rootPath(fileOrZip: Path): Path = {
    if (fileOrZip.getFileName.toString.endsWith(".jar")) {
      val uri = URI.create("jar:file:" + fileOrZip.toUri.getPath)
      newFileSystem(uri, new ju.HashMap[String, Any]()).getPath("/")
    } else fileOrZip
  }

  private def newFileSystem(uri: URI, map: ju.Map[String, Any]) =
    try FileSystems.newFileSystem(uri, map)
    catch { case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri) }

  def deleteRecursive(p: Path): Unit = {
    Files.walkFileTree(p, new SimpleFileVisitor[Path]() {
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
