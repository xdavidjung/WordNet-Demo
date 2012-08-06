package knowitall.wordnetdemo

import scala.collection.JavaConverters._
import knowitall.tool.{ WNDictionary, Converter }
import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker
import edu.washington.cs.knowitall.nlp.extraction.ChunkedBinaryExtraction
import edu.washington.cs.knowitall.tool.chunk.{ OpenNlpChunker, ChunkedToken }
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import edu.mit.jwi.morph.WordnetStemmer
import edu.mit.jwi.Dictionary


object WNMain {
  
  val usage = "[-s] [-mh] [-a1] [-a2] [-wn] sentence"
  val defaultSense = 0
  val defaultHeight = 3
  type OptionMap = Map[Symbol, Any]

  var jwi: JwiTools = null
  var dict: Dictionary = null
  var stemmer: WordnetStemmer = null
  val reverb = new ReVerbExtractor()
  
  def main(args: Array[String]) {

    // parse args
    val argsMap = mapArgs(args)
    
    // if the user passes in a directory, dictionary will be set to it; else empty
    val dictionary = if (!argsMap.contains('wn)) ""
                     else argsMap.get('wn).get.toString
    
    jwi = new JwiTools(dictionary)
    dict = jwi.dict
    stemmer = jwi.stemmer
    
    // the sentence to tokenize and get args from
    val sentence = argsMap.get('sentence).get.toString
    
    // the sense index to fetch from wordnet
    val sense = if (!argsMap.contains('s)) defaultSense
                else argsMap.get('s).get.asInstanceOf[Int]
    
    // how many levels of hypernyms to fetch
    val hypHeight = if (!argsMap.contains('mh)) defaultHeight 
                    else argsMap.get('mh).get.asInstanceOf[Int] + 1
                    
    // whether to get hypernyms for arg1, arg2
    val useArg1 = argsMap.contains('a1)
    val useArg2 = argsMap.contains('a2)

    val reverb = new ReVerbExtractor
    val tokens = reverb.extractFromString(sentence).iterator.next

    /* Takes a sequence of postagged tokens and prints out the hypernym
     * stream.
     */
    def printStream(tokens: Seq[PostaggedToken]): Unit = {
      
      val stream = jwi.posTokensToHypernymStream(tokens, sense) take hypHeight
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
    if (useArg1) printStream(Converter.CAEToPTTs(tokens, ChunkedBinaryExtraction.ARG1))
    if (useArg2) printStream(Converter.CAEToPTTs(tokens, ChunkedBinaryExtraction.ARG2))
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
        case "-wn" :: value :: tail =>
          nextOption(map ++ Map('wn -> value.toString), tail)
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