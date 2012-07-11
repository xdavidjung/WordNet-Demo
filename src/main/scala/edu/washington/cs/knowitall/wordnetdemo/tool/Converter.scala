package knowitall.tool
import edu.washington.cs.knowitall.nlp.extraction.ChunkedArgumentExtraction
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import edu.washington.cs.knowitall.nlp.extraction.ChunkedBinaryExtraction

/** Converts from ChunkedBinaryExtraction objects to sequences of 
  * PostaggedTokens. 
  */
object Converter {

  /** This function takes a chunked binary extraction from ReVerb and creates 
    * a sequence of POS tagged tokens out of either the arg1 or arg2. 
    * 
    * Note that this will not preserve argument offsets. 
    * 
    * @requires the cbe to be well-formed, i.e. from ReVerb.
    * @param cbe
    * @param arg which argument to return, ARG1 or ARG2.
    * @return a sequence of PostaggedToken objects corresponding to the 
    *         appropriate arg from cbe. 
    */
  def CAEToPTTs(cbe: ChunkedBinaryExtraction, arg: String): Seq[PostaggedToken] = {
    
    val cae = if (arg == "ARG1") cbe.getArgument1
              else if (arg == "ARG2") cbe.getArgument2 
              else throw new IllegalArgumentException
              
    val tags = cae.getPosTags.iterator()
    val sentence = cae.getText.split(" ")
    
    // bit of a hack because we give each token an offset of 0.
    sentence.map(token => new PostaggedToken(tags.next, token, 0))
  }
  
}