import scala.collection.immutable.ListMap


class Processor {

  def process(str: String): String = {
    if (isEmpty(str)) {
      throw new IllegalArgumentException("String cant be blank")
    }

    val m = ListMap(str.trim.replaceAll(" +", " ").toLowerCase.split(" ") // format
      .groupBy(_.charAt(0)) // group by first char
      .toSeq.sortBy(_._1): _*) // sort by first char
      .filter((t) => t._2.length > 1) // remove groups with one word
      .map { case (k, v) => (k, v.toSeq.sortWith(sortByLengthCustom)) } // sort by length and alphabetical order

    m.map { case (k, v) => k + "=" + v.toList.mkString("[", ", ", "]") }.mkString("[", ", ", "]")
  }

  def sortByLengthCustom(s1: String, s2: String): Boolean = {
    if (s1.length != s2.length) {
      s1.length > s2.length
    } else {
      s1 < s2
    }
  }

  def isEmpty(x: String): Boolean = x == null || x.trim.isEmpty
}