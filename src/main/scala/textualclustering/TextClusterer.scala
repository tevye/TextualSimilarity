package textualclustering

object TextClusterer {
  type nGram = String
  type Occurrences = List[(String, Int)]
  def merge(a: Map[nGram, List[(String,Int)]], b: Map[nGram, List[(String,Int)]]): Map[nGram, List[(String,Int)]] = {
    def ad = a withDefaultValue List()
    ad ++ b.map{ case (k,v) => k -> (v ::: ad(k)) }
  }
  def nGramMaker(s: String, n: Int): List[String] = {
    def stmp = s.replaceAll("[\\s]", "")
     (for ( i <- 0 until stmp.length - (n - 1)) yield stmp.toLowerCase.substring(i, i + n)) toList
  }
  def gramOccurrences(grams: List[String]): List[(nGram, Int)] = {
    (grams groupBy identity map { case(k,v) => (k, v.length)} toList).sortBy((x => x._1))
  }
  def setupIndex(s: String, g: Int): Map[nGram, List[(String,Int)]] = {
     for ( (k,v) <- (gramOccurrences(nGramMaker(s, g))).toMap ) yield (k -> List((s.toLowerCase, v)))
  }
  def sharedGrams(s: String, ref: Map[nGram, List[(String,Int)]]): List[String] = {
    ((for (x <- (for ((n,c) <- gramOccurrences(nGramMaker(s, 3))) yield ref(n)) flatten) yield x._1) toSet) toList
  }  
  def weighted(s: String, ref: Map[nGram, List[(String,Int)]]): List[(String, Double)] = {
    def targets = sharedGrams(s, ref)
    (for (t <- targets) yield (t, similarity(s, t))) sortWith (_._2 > _._2)
  }
  def partialLikeness(a: String, b: String): Double = {
    def aList = (gramOccurrences(nGramMaker(a, 3)) toMap) withDefaultValue  0
    def bList = (gramOccurrences(nGramMaker(b, 3)) toMap) withDefaultValue  0
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
}
