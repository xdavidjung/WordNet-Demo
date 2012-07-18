package knowitall.wordnetdemo

import scala.collection.JavaConverters._
import knowitall.tool.{ WNDictionary, Converter }
import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker
import edu.washington.cs.knowitall.nlp.extraction.ChunkedBinaryExtraction
import edu.washington.cs.knowitall.tool.chunk.{ OpenNlpChunker, ChunkedToken }
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import edu.mit.jwi.morph.WordnetStemmer


object WNMain {
  
  val usage = "[-s] [-mh] [-a1] [-a2] sentence"
  val defaultSense = 0
  val defaultHeight = 3
  type OptionMap = Map[Symbol, Any]

  val dict = WNDictionary.fetchDictionary()
  val stemmer = new WordnetStemmer(dict)
  val reverb = new ReVerbExtractor()
  
  def main(args: Array[String]) {

    // parse args
    val argsMap = mapArgs(args)
    val sentence = argsMap.get('sentence).get.toString
    val sense = if (!argsMap.contains('s)) defaultSense
                else argsMap.get('s).get.asInstanceOf[Int]
    val hypHeight = if (!argsMap.contains('mh)) defaultHeight 
                    else argsMap.get('mh).get.asInstanceOf[Int] + 1
    val arg1 = argsMap.contains('a1)
    val arg2 = argsMap.contains('a2)

    val reverb = new ReVerbExtractor
    val tokens = reverb.extractFromString(sentence).iterator.next

    /* Takes a sequence of postagged tokens and prints out the hypernym
     * stream.
     */
    def printStream(tokens: Seq[PostaggedToken]): Unit = {
      
      val stream = JwiTools.posTokensToHypernymStream(tokens, sense) take hypHeight
      if (stream == Stream(Set())) {
        println("Noun not found.")
        return
      }
      
      for (i <- 0 to hypHeight - 1) {
        print("Height "+ (i + 1) +": ")
        stream(i).foreach(synset => {
          val strings = synset.getWords.asScala.map(s => s.getLemma)
          print(strings.mkString(", ").replace("_", " "))
          print("; ")
        })
        println()
      }
      println()
    }
    if (arg1) printStream(Converter.CAEToPTTs(tokens, ChunkedBinaryExtraction.ARG1))
    if (arg2) printStream(Converter.CAEToPTTs(tokens, ChunkedBinaryExtraction.ARG2))
  }

  /*
   * Takes in arguments to WordNetDemo and parses them into a map.
   *
   * @requires args must adhere to usage string.
   * @param args an array of argument strings.
   * @returns a map from accepted arguments to their values.
   */
  def mapArgs(args: Array[String]): OptionMap = {
    if (args.length == 0) {
      println(usage)
      exit
    }

    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    /* This recursive function initially takes an empty OptionMap and 
     * a list of argument strings and fills the map with the values 
     * specified in the string.
     */
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "-s" :: value :: tail =>
          nextOption(map ++ Map('s -> value.toInt), tail)
        case "-mh" :: value :: tail =>
          nextOption(map ++ Map('mh -> value.toInt), tail)
        case "-a1" :: tail =>
          nextOption(map ++ Map('a1 -> true), tail)
        case "-a2" :: tail =>
          nextOption(map ++ Map('a2 -> true), tail)
        case string :: tail =>
          nextOption(
            map ++ Map('sentence -> string),
            tail)
        case _ => map
      }
    }

    nextOption(Map(), arglist)
  }
  
}