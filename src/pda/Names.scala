package pda

import scala.collection.mutable

class Names {
  private val used = mutable.Map[String, Int]()

  def apply(key: String): String = {
    require(!key.isBlank)
    require(!key.last.isDigit)

    if(used.contains(key)) {
      val result = s"$key${used(key)}"
      used(key) += 1
      result
    } else {
      used(key) = 0
      key
    }
  }
}
