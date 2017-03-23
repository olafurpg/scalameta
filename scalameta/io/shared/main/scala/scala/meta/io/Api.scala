package scala.meta.io

private[meta] trait Api {
  implicit class XtensionAbsolutePath(path: AbsolutePath) {
    def read: String = PlatformIO.read(path)
  }
}

private[meta] trait Aliases
