package knowitall.wordnetdemo

import java.net.URL

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{ POS, Pointer, ISynsetID, ISynset }
import edu.mit.jwi.morph.WordnetStemmer
import edu.mit.jwi.data.ILoadPolicy

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import knowitall.tool.WNDictionary

/** TODO: This.
 */
class WordNetDemoMain {

  /** Dict is an opened JWI dictionary object. */
  val dict = WNDictionary.fetchDictionary()
  /** Stemmer is a JWI WordnetStemmer object. */
  val stemmer = new WordnetStemmer(dict)

  /** Goes from a string word to a hypernym stream of the word's nth noun sense.
   * 
   * @requires str exists in WordNet with an nth noun sense. 
   * @param str the noun to look up in WordNet
   * @param n the sense to look up
   * @returns a Stream of Buffers of ISynsets i where each i is a hypernym
   *          of a ISynset in the argument buffer, and each level in the
   *          stream returns one deeper level in the hypernym hierarchy. 
   */
  def stringToHypernymStream(str: String, n: Int): Stream[Buffer[ISynset]] = {
    val stemmedStr = stem(str, 0)
    val nthSynset = stringToNthSynset(stemmedStr, n)
    val hypernyms = synsetToHypernyms(nthSynset)
    
    hypernymStream(hypernyms)
  }
  
  /** Goes from a string word to the synset of its nth noun sense. 
   * 
   * @requires str is a noun in WordNet with an nth sense. 
   * @param str the word to query in Wordnet.
   * @param n the sense to look up.
   * @return the synset of str's nth sense. 
   */
  def stringToNthSynset(str: String, n: Int): ISynset = {
    val strStem = stem(str, 0)
    val idxWord = dict.getIndexWord(strStem, POS.NOUN)
    val wordID = idxWord.getWordIDs.get(n)
    val wnWord = dict.getWord(wordID)
    wnWord.getSynset
  }
  
  /** Goes from a synset to a Buffer of its hypernyms (which are
   * also synsets).
   * 
   * @param synset the synset to get hypernyms for.
   */
  def synsetToHypernyms(synset: ISynset): Buffer[ISynset] = {
    val hypernymIDs = synset.getRelatedSynsets(Pointer.HYPERNYM).asScala
    hypernymIDs.map(sid => dict.getSynset(sid))
  }

  /** Gets a stream of hypernym sets. 
   * 
   * @param synsets a Buffer of ISynsets.
   * @returns a Stream of Buffers of ISynsets where each ISynset is a hypernym
   *          of a ISynset in the argument buffer, and each level in the
   *          stream returns one deeper level in the hypernym hierarchy. 
   */
  def hypernymStream(synsets: Buffer[ISynset]): Stream[Buffer[ISynset]] = {
    val hypernyms = synsets flatMap synsetToHypernyms
    synsets #:: hypernymStream(hypernyms)
  }

  /** Find a word's corresponding WordNet stem.
   * 
   * @requires word must be a noun in the WordNet dictionary.
   * @param word the word to find the stem of.
   * @param n which stem to return - set this to 0, rarely will you want others.
   * @return word's nth WordNet stem.
   */
  def stem(word: String, n: Int): String = {
    return stemmer.findStems(word, POS.NOUN).asScala(n)
  }
}