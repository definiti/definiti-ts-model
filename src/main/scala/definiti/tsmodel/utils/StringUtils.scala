package definiti.tsmodel.utils

object StringUtils {
  def lastPart(source: String, separator: Char = '.'): String = {
    if (source.isEmpty) {
      source
    } else if (source.last == separator) {
      lastPart(source.substring(0, source.length - 1), separator)
    } else if (source.contains(separator)) {
      source.substring(source.lastIndexOf(separator) + 1)
    } else {
      source
    }
  }

  def excludeLastPart(source: String, separator: Char = '.'): String = {
    if (source.isEmpty) {
      source
    } else if (source.last == separator) {
      source.substring(0, source.length - 1)
    } else if (source.contains(separator)) {
      source.substring(0, source.lastIndexOf(separator))
    } else {
      ""
    }
  }
}
