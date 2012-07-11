# WordNet-Demo

This project works with WordNet through the JWI library (via MIT) to find types for nouns. 

The main class can be found in src/main/scala/edu/washington/cs/WNMain.scala. 


## Usage

"[-s] [-mh] [-a1] [-a2] sentence"

Note that -s and -mh default to 0 and 3, respectively, but -a1 and -a2 must be specified. If they are both specified, types for argument one are returned first, followed by the types for argument 2. 

Note that "sentence" must be a complete English sentence containing a binary relationship (containing a noun subject, a verb relationship, and a noun subject). 

Sample inputs: 

    "-s 0 -mh 4 -a1 "The moment of inertia is a physics concept.""

    "-s 0 -mh 1 -a1 -a2 "Newton's law of motion has been around for centuries."" 


Options:

    -s <arg>   Specifies which "sense", or definition, of the word to use.
               For example, a word like "dog" can be used to refer to the 
               household pet or mammal, as in the sentence "Joey's new dog,
               a golden retriever, liked to eat everything in sight." 
               Its second sense might be for when it's used as a synonym to 
               the word "cad", as in the sentence "Kaleb was such a dog, 
               picking up and disposing of a new lady-friend nightly." 

               In general, the 0th sense will be a word's primary definition.

    -mh <arg>  How many levels of types to find. 
               For example, the first "level" for a word like "dog" might consist
               of "canine" and "household pet", and the second level might be 
               "mammal" and "pet". 

    -a1        Whether to return types for the first argument in the binary 
               relationship extracted. 

    -a2        Whether to return types for the second argument in the binary
               relationship extracted.
