import textualclustering._

object ScratchPad {
  println("Welcome to the Scala worksheet")
  
  def s1 = "abcABCdefg"
  def s2 = "cdefghi"

  def s1g = (TextClusterer.gramOccurrences(TextClusterer.nGramMaker(s1, 3)) toMap) withDefaultValue  0
  def s2g = (TextClusterer.gramOccurrences(TextClusterer.nGramMaker(s2, 3)) toMap) withDefaultValue  0
  println(s1g)
  println(s2g)
  def tt = s1g filterNot { case(k, v) => s2g.contains(k) }
  
  println(tt)
  println(tt size)
   def partialLikeness(a: String, b: String): Double = {
    def aList = (TextClusterer.gramOccurrences(TextClusterer.nGramMaker(a, 3)) toMap) withDefaultValue  0
    def bList = (TextClusterer.gramOccurrences(TextClusterer.nGramMaker(b, 3)) toMap) withDefaultValue  0
    def diff = aList filterNot { case(k, v) => b.contains(k) }
    ((aList size) - (diff size)) / (1.0 * (aList size))
  }

  def similarity(a: String, b: String): Double = (a, b) match {
    case (null,    null)   => 1.0
    case (null,    b2)     => 0.0
    case (a2,      null)   => 0.0
    case (z,       w)      => {
      if (z.equals(w)) 1.0
      else partialLikeness(a, b) * partialLikeness(b, a)
    }
  }
  
  println(similarity(s1, s2))
  println(similarity(s1, s1))
  println(TextClusterer.similarity("abcdefghijk", "abcdefghijklmnop"))
}