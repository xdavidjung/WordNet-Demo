package knowitall.tool

object WNDictionary {

  import java.net.URL
  import edu.mit.jwi.Dictionary
  import java.io.FileNotFoundException

  /* Gets a WordNet dictionary.
   *
   * @requires there to be a environment variable WNHOME that maps to
   *           a directory containing the dictionary.
   * @returns an opened dictionary object.
   */
  def fetchDictionary(): Dictionary = {

    val wnhome = System.getenv("WNHOME")
    if (wnhome == null) {
      println("Environment variable WNHOME must be set to the root " +
          "directory of the WordNet installation.")
      exit
    }
    val path = wnhome + "/dict"
    val url = new URL("file", null, path)

    val dict = new Dictionary(url)
    val success = dict.open()
    if (success) dict else throw new FileNotFoundException()
    return dict;
  }
  
}