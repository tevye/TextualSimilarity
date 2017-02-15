package textualclustering

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import TextClusterer._

@RunWith(classOf[JUnitRunner])
class TextClustererSuite extends FunSuite  {

  def text = "The quick red fox jumped over the lazy dog"
  
  def gramOccMapResult = Map("mpe" -> 1, "zyd" -> 1, "qui" -> 1, "hel" -> 1, "ump" -> 1, "ert" -> 1, "dog" -> 1, "ped" -> 1
      , "rth" -> 1, "ydo" -> 1, "edo" -> 1, "ove" -> 1, "ick" -> 1, "edf" -> 1, "laz" -> 1
      , "azy" -> 1, "xju" -> 1, "dfo" -> 1, "ver" -> 1, "heq" -> 1, "equ" -> 1, "ckr" -> 1, "dov" -> 1
      , "jum" -> 1, "kre" -> 1, "fox" -> 1, "oxj" -> 1, "uic" -> 1, "red" -> 1, "the" -> 2
      , "ela" -> 1)
 
  test("nGramMaker (naive base test): abcd") {
    assert(nGramMaker("abcd", 3) === List("abc", "bcd"))
  }

  test("nGramMaker (ignore whitespace characters): ab\t   cd") {
    assert(nGramMaker("abcd", 3) === List("abc", "bcd"))
  }
  test("loadDictionary") {
    assert(loadDictionary.length > 0)
  }


}
