## TextualSimilarity
Small scala program to assign a similarity score of two strings based on tri-gram analysis.

### Kudos to Martin Odersky and the others who delivered Scala training
This small program was inspired by the course work given in the class "Functional Programming Principles in Scala"
by École Polytechnique Fédérale de Lausanne on [Coursera](https://www.coursera.org).

### Description
Take a string, force it to lowercase, remove whitespaces, and generate a list of tri-grams (three symbol sub-strings) and the number of times the tri-gram appears in the string. Given a corpus of some form (a list of words apparently recognized in linux is in the resources folder); build an index of words that share tri-grams and then be able to find and score the similarity of the appropriate words in the corpus to a chosen word.

### Sub-optimal implementaion detail (in the initial code)
While the number of times a given tri-gram is captured, the initial similarity score ignores that fact. *Hope someone else contributes an enhancement.*

### Example run
```
> run
[info] Compiling 1 Scala source to /Users/stevelu/scala/TextualClustering/target/scala-2.11/classes...
[warn] there were 10 feature warnings; re-run with -feature for details
[warn] one warning found
[info] Running textualclustering.DemoApp 
Finding words that share tri-grams with 'zoo':
(zoo,1.0)
(zoos,0.5)
(zoom,0.5)
(zooms,0.3333333333333333)
(kalamazoo,0.14285714285714285)
(zoological,0.125)
(zoologically,0.1)
[success] Total time: 12 s, completed Feb 15, 2017 9:57:04 AM
```
