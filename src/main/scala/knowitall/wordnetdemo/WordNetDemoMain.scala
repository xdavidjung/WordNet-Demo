package knowitall.wordnetdemo

/** This class is the main class for a program that takes a full sentence,
  * 
  * 
  */
object WordNetDemoMain {

  import java.net.URL

  import edu.mit.jwi.Dictionary
  import edu.mit.jwi.item.{POS, Pointer, ISynsetID, ISynset}
  import edu.mit.jwi.morph.WordnetStemmer
  
  import scala.collection.JavaConverters._
  import scala.collection.mutable.Buffer

  import edu.washington.cs.knowitall.tool.chunk.{OpenNlpChunker, ChunkedToken}

  val usage = "wndemo [-s] [-mh] sentence index1 index2"
  val defaultSense = 0;
  type OptionMap = Map[Symbol, Any]

  val dict = fetchDictionary()
  val stemmer = new WordnetStemmer(dict)

  def main(args: Array[String]) {

    // parse args
    val argsMap = mapArgs(args)
    val sentence = argsMap.get('sentence).get.toString()
    val ind1 = argsMap.get('index1).get.asInstanceOf[Int]
    val ind2 = argsMap.get('index2).get.asInstanceOf[Int]
    
    val chunker = new OpenNlpChunker
    val tokens = chunker.chunk(sentence)

    // nounPhrase contains only nouns, but no proper nouns. 
    val nounPhrase = tokens.filter(t => t.postag.head == 'N' && 
                                        t.postag != "NNP")
                                        
    // TODO: this will go over all the nouns - we don't want this. 
    for(noun <- nounPhrase) {
      // get stem
      val stemmedWord = stem(noun.string, 0)

      // get hypernyms
      val hypernyms = getHypernyms(stemmedWord, 0)
      
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

  /* Goes from a string word to the synset of its nth sense. 
   * 
   * @param str the word to query in Wordnet.
   * @param n the sense to look up.
   * @return the synset of str's nth sense. 
   */
  def stringToNthSynset(str: String, n: Int): ISynset = {
    val strStem = stem(str, n)
    val idxWord = dict.getIndexWord(strStem, POS.NOUN)
    val wordID = idxWord.getWordIDs.get(n) 
    val wnWord = dict.getWord(wordID)
    wnWord.getSynset
  }
  
  /* Goes from a synset to a Buffer of its hypernyms (which are
   * also synsets).
   * 
   * @param synset the synset to get hypernyms for.
   */
  def synsetToHypernyms(synset: ISynset): Buffer[ISynset] = {
    val hypernymIDs = synset.getRelatedSynsets(Pointer.HYPERNYM).asScala
    hypernymIDs.map(sid => dict.getSynset(sid))
  }
  
  /*
   * 
   */
  def hypernymStream(synsets: Buffer[ISynset]): Stream[Buffer[ISynset]] = {
    val hypernyms = synsets flatMap synsetToHypernyms
    synsets #:: hypernymStream(hypernyms)
  }
  
  /* Gets a buffer of synsetIDs corresponding to hypernym sets for word.
   * 
   * @requires nth sense of word must be defined in Wordnet.
   * @param word the word to find the hypernyms of
   * @param nthSense defines which sense (defn) to get the hypernyms of
   * @returns a Buffer of hypernyms for the nthSense of word.
   */
  def getHypernyms(word: String, nthSense: Int): Buffer[ISynsetID] = {
    // fetch synset
    val idxWord = dict.getIndexWord(word, POS.NOUN)
    val wordID = idxWord.getWordIDs.get(nthSense) 
    val wnWord = dict.getWord(wordID)
    val synset = wnWord.getSynset
    
    synset.getRelatedSynsets(Pointer.HYPERNYM).asScala
  }
  
  /* Find a word's stem.
   * 
   * @requires word must be a noun in the Wordnet dictionary.
   * @param word the word to stem
   * @param n which stem to return - usually 0.
   * @return word's nth Wordnet stem.
   */
  def stem(word: String, n: Int): String = {
    return stemmer.findStems(word, POS.NOUN).asScala(n)
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

  /* Gets a WordNet dictionary.
   *
   * @requires there to be a environment variable WNHOME that maps to
   *           a directory containing the dictionary.
   * @returns an opened dictionary object.
   */
  def fetchDictionary(): Dictionary = {

    val wnhome = System.getenv("WNHOME")
    val path = wnhome + "/dict";
    val url = new URL("file", null, path);

    val dict = new Dictionary(url);
    dict.open();
    return dict;
  }

}