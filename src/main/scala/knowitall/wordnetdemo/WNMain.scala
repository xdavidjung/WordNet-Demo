package knowitall.wordnetdemo

import knowitall.tool.WNDictionary

import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker;
import edu.washington.cs.knowitall.nlp.extraction.ChunkedBinaryExtraction;

import edu.washington.cs.knowitall.tool.chunk.{ OpenNlpChunker, ChunkedToken }
import edu.mit.jwi.morph.WordnetStemmer

object WNMain {
  
  val usage = "hypernyms [-s] [-mh] sentence"
  val defaultSense = 0;
  type OptionMap = Map[Symbol, Any]

  val dict = WNDictionary.fetchDictionary()
  val stemmer = new WordnetStemmer(dict)
  val reverb = new ReVerbExtractor()

  def main(args: Array[String]) {

    // parse args
    val argsMap = mapArgs(args)
    val sentence = argsMap.get('sentence).get.toString
    val senses = argsMap.get('s).get.asInstanceOf[Int]
    val hypHeight = argsMap.get('mh).get.asInstanceOf[Int]

    val reverb = new ReVerbExtractor
    val tokens = reverb.extractFromString(sentence)

    //TODO: tokens is a Seq[ChunkedToken] and each of these tokens has a POS tag. 
    //      We want to get only the ones that are noun phrases, but not proper nouns.
    //      Then for each of these noun phrases, we want to search for the longest
    //      combination of words that is in WordNet, tiebreaking by largest offset.
    
    // nounPhrase contains only nouns, but no proper nouns. 
    val nounPhrase = tokens.filter(t => t.postag.head == 'N' &&
      t.postag != "NNP" && t.postag != "NNPS")

    // TODO: this will go over all the nouns - we don't want this. 
    for (noun <- nounPhrase) {

      jwi.stringToHypernymStream(noun.string, 0)

      // print them out
      for (sid <- hypernyms) {
        var words = dict.getSynset(sid).getWords
        print(sid + " {")
        var iterator = words.iterator
        while (iterator.hasNext) {
          print(iterator.next.getLemma)
          if (iterator.hasNext)
            print(", ")
        }
        println("}")
      }
    }
  }

  /*
   * Takes in arguments to WordNetDemo and parses them into a map.
   *
   * @requires args must adhere to usage string.
   * @param args an array of argument strings.
   * @returns a map from accepted arguments to their values.
   */
  def mapArgs(args: Array[String]): OptionMap = {
    if (args.length == 0) println(usage)

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
        case string :: index1 :: index2 :: tail =>
          nextOption(
            map ++ Map('sentence -> string) ++
              Map('index1 -> index1.toInt) ++
              Map('index2 -> index2.toInt),
            tail)
        case _ => map
      }
    }

    nextOption(Map(), arglist)
  }
  
}