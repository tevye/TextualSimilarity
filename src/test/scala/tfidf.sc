package test.scala

import textualclustering._

object tfidf {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  type nGram = String
  type Occurrences = List[(String, Int)]
  def merge(a: Map[nGram, List[(String,Int)]], b: Map[nGram, List[(String,Int)]]): Map[nGram, List[(String,Int)]] = {
    def ad = a withDefaultValue List()
    ad ++ b.map{ case (k,v) => k -> (v ::: ad(k)) }
  }                                               //> merge: (a: Map[test.scala.tfidf.nGram,List[(String, Int)]], b: Map[test.scal
                                                  //| a.tfidf.nGram,List[(String, Int)]])Map[test.scala.tfidf.nGram,List[(String, 
                                                  //| Int)]]
  class GramToSourceCount(val v: Map[nGram, List[(String,Int)]]) {
    def + (b: GramToSourceCount): GramToSourceCount = {
      new GramToSourceCount(merge(v, b.v))
    }
    override def toString = v.toString
  }
  def text = "The quick red fox jumped over the lazy dog"
                                                  //> text: => String
  def squeezed = text.toLowerCase().replaceAll("[\\s]", "")
                                                  //> squeezed: => String
  var tri = for {
    i <- 0 until (squeezed.length - 2)
  } yield squeezed.substring(i,i+3)               //> tri  : scala.collection.immutable.IndexedSeq[String] = Vector(the, heq, equ,
                                                  //|  qui, uic, ick, ckr, kre, red, edf, dfo, fox, oxj, xju, jum, ump, mpe, ped, 
                                                  //| edo, dov, ove, ver, ert, rth, the, hel, ela, laz, azy, zyd, ydo, dog)

  def nGramMaker(s: String, n: Int): List[String] = {
     (for ( i <- 0 until (s.length - (n - 1))) yield s.toLowerCase.substring(i, i + n)) toList
  }                                               //> nGramMaker: (s: String, n: Int)List[String]
  
  //nGramMaker(squeezed, 3)
  //nGramMaker(squeezed, 4)
  
  def gramOccurrences(grams: List[String]): List[(nGram, Int)] = {
    (grams groupBy identity map { case(k,v) => (k, v.length)} toList).sortBy((x => x._1))
  }                                               //> gramOccurrences: (grams: List[String])List[(test.scala.tfidf.nGram, Int)]
  def setupIndex(s: String, g: Int): Map[nGram, List[(String,Int)]] = {
     for ( (k,v) <- (gramOccurrences(nGramMaker(s, g))).toMap ) yield (k -> List((s.toLowerCase, v)))
  }                                               //> setupIndex: (s: String, g: Int)Map[test.scala.tfidf.nGram,List[(String, Int
                                                  //| )]]

  /*
  */
  
  // (List[(nGram, Int)], (nGram -> List[String]))

  def gramCheck = gramOccurrences(tri toList)     //> gramCheck: => List[(test.scala.tfidf.nGram, Int)]
  //println(gramCheck)
  gramCheck toMap                                 //> res0: scala.collection.immutable.Map[test.scala.tfidf.nGram,Int] = Map(mpe 
                                                  //| -> 1, zyd -> 1, qui -> 1, hel -> 1, ump -> 1, ert -> 1, dog -> 1, ped -> 1,
                                                  //|  rth -> 1, ydo -> 1, edo -> 1, ove -> 1, ick -> 1, edf -> 1, laz -> 1, azy 
                                                  //| -> 1, xju -> 1, dfo -> 1, ver -> 1, heq -> 1, equ -> 1, ckr -> 1, dov -> 1,
                                                  //|  jum -> 1, kre -> 1, fox -> 1, oxj -> 1, uic -> 1, red -> 1, the -> 2, ela 
                                                  //| -> 1)
  def s1 = "abcABCdefg"                           //> s1: => String
  def s2 = "cdefghi"                              //> s2: => String
  def s3 = "lmnop"                                //> s3: => String
  def s4 = "bcdnop"                               //> s4: => String
  def sss = List(s1,s2,s3,s4)                     //> sss: => List[String]
  
  def sssList = for (s <- sss) yield setupIndex(s,3)
                                                  //> sssList: => List[Map[test.scala.tfidf.nGram,List[(String, Int)]]]
  def corp = sssList reduceLeft ((x,y) => merge(x,y))
                                                  //> corp: => Map[test.scala.tfidf.nGram,List[(String, Int)]]
  
  for (x <- corp) yield println(x)                //> (efg,List((cdefghi,1), (abcabcdefg,1)))
                                                  //| (abc,List((abcabcdefg,2)))
                                                  //| (nop,List((bcdnop,1), (lmnop,1)))
                                                  //| (lmn,List((lmnop,1)))
                                                  //| (mno,List((lmnop,1)))
                                                  //| (fgh,List((cdefghi,1)))
                                                  //| (dno,List((bcdnop,1)))
                                                  //| (cab,List((abcabcdefg,1)))
                                                  //| (cde,List((cdefghi,1), (abcabcdefg,1)))
                                                  //| (cdn,List((bcdnop,1)))
                                                  //| (def,List((cdefghi,1), (abcabcdefg,1)))
                                                  //| (bcd,List((bcdnop,1), (abcabcdefg,1)))
                                                  //| (ghi,List((cdefghi,1)))
                                                  //| (bca,List((abcabcdefg,1)))
                                                  //| res1: scala.collection.immutable.Iterable[Unit] = List((), (), (), (), (), 
                                                  //| (), (), (), (), (), (), (), (), ())
  //def s1ix = new GramToSourceCount(setupIndex(s1, 3))
  //def s2ix = new GramToSourceCount(setupIndex(s2, 3))
  //println(s1ix.v)
  //def r = s1ix + s2ix
  //println(r)
  
  def nums = List(1,2,3,4,5)                      //> nums: => List[Int]
  def addEm(a: Int, b: Int): Int = a + b          //> addEm: (a: Int, b: Int)Int
 
  //def entries = for (s <- loadDictionary) yield setupIndex(s, 3)
  
  //def corpus = entries reduceLeft ((x,y) => merge(x,y))
  def sharedGrams(s: String, ref: Map[nGram, List[(String,Int)]]): Set[String] = {
    (for (x <- (for ((n,c) <- gramOccurrences(nGramMaker(s, 3))) yield ref(n)) flatten) yield x._1) toSet
  }                                               //> sharedGrams: (s: String, ref: Map[test.scala.tfidf.nGram,List[(String, Int)
                                                  //| ]])Set[String]
  
  def cpath = System.getProperty("java.class.path").split(":")
                                                  //> cpath: => Array[String]
  
  cpath filter (_.contains("resource"))           //> res2: Array[String] = Array(/Library/Java/JavaVirtualMachines/jdk1.8.0_65.j
                                                  //| dk/Contents/Home/jre/lib/resources.jar)
  
  def partialLikeness(a: String, b: String): Double = {
    //def aList = gramOccurrences(nGramMaker(a, 3)) withDefaultValue  0
    //def bList = gramOccurrences(nGramMaker(b, 3)) withDefaultValue  0
    //def diff = aList filterNot
    0.0
  }                                               //> partialLikeness: (a: String, b: String)Double
  for (k <- sharedGrams("mnop", corp) toList) yield println(k)
                                                  //> lmnop
                                                  //| bcdnop
                                                  //| res3: List[Unit] = List((), ())
  def similarity(a: String, b: String): Double = (a, b) match {
    case (null,    null)   => 1.0
    case (null,    b2)     => 0.0
    case (a2,      null)   => 0.0
    case (z,       w)      => {
      if (z.equals(w)) 1.0
      else partialLikeness(a, b) * partialLikeness(b, a)
    }
  }                                               //> similarity: (a: String, b: String)Double
  
  
}