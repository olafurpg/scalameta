package scala.meta.internal.semantic.vfs

import java.io._
import org.scalameta.data._
import scala.meta.io._
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.io.PathIO.fileSeparator
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}

object Database {
  def load(classpath: Classpath): v.Database = {
    val fentries = classpath.deep.filter(e => v.Paths.isSemanticdb(e.name))
    val ventries = fentries.map(fentry => v.Entry.OnDisk(fentry))
    v.Database(ventries)
  }
}

@data
class Database(entries: List[Entry]) {
  def toSchema: s.Database = {
    val sentries = entries.map(ventry => s.Attributes.parseFrom(ventry.bytes))
    s.Database(sentries)
  }

  def save(): Unit = {
    entries.foreach(ventry => {
      val file = new File(ventry.uri)
      file.getParentFile.mkdirs()
      val fos = new FileOutputStream(file)
      try fos.write(ventry.bytes)
      finally fos.close()
    })
  }
}
