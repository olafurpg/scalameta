package scala.meta.internal.prettyprinters

final class PrettySettings private (
    val maxColumn: Int
) {
  private def this() = {
    this(
      maxColumn = 80
    )
  }

  def withMaxColumn(maxColumn: Int): PrettySettings = {
    copy(maxColumn = maxColumn)
  }

  private def copy(
      maxColumn: Int = maxColumn
  ): PrettySettings = {
    new PrettySettings(
      maxColumn = maxColumn
    )
  }
}

object PrettySettings {
  def apply(): PrettySettings = {
    new PrettySettings()
  }
}
