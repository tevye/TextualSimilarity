package textualclustering

object DemoApp extends App {
    println("Finding words that share tri-grams with 'zoo':")
    //def text = "The quick red fox jumped over the lazy dog"
    //println("trigrams: " + TextClusterer.nGramMaker(text, 3))
    //for (p <- System.getProperty("java.class.path").split(":")) println("Classpath element: " +p)
    def entries = for (s <- loadDictionary) yield TextClusterer.setupIndex(s, 3)
    def corpus = entries reduceLeft ((x,y) => TextClusterer.merge(x,y))
    for (zz <- TextClusterer.weighted("zoo", corpus)) yield println(zz)
}