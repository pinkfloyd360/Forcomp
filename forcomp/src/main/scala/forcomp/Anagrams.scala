package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val chars = w.toLowerCase.toList.sorted
    //println(chars + " " + chars.mkString)
    
    val y = chars.groupBy(x => chars.count(y => y.equals(x)))
    
    val rev = for {
      (digit, str) <- y
      ltr <- str
    } yield (ltr, digit)
    
    val p = for{
      character: Char <- chars.sorted.toSet
      digit <- rev.get(character)
    } yield (character, digit)
    
    p.toList.sortBy(_._1)
    
    
    
    
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)
  
 

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
  	dictionary groupBy (wordOccurrences(_))
  
  def wordAnagramsContains(occ: Occurrences): Boolean = {
    dictionaryByOccurrences.get(occ) match {
      case Some(list) => {
        //println(list)
        true
      }
      case None => false
    }
  }
  
  def wordFromDictionary(occ: Occurrences): Word = {
    dictionaryByOccurrences.get(occ) match {
      case Some(list) => {
        //println(list)
        list.head
      }
      
    }
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences.get(wordOccurrences(word)) match {
      case Some(list) => list
      case None => List[Word]()
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    
	val p = ((for {
      item <- occurrences;
      nums <- 1 to item._2
    } yield (item._1, nums)) groupBy (_._1)).values.toList
    
    
	
	// get the first element in the list
	
	def comb(a: List[List[(Char, Int)]], b: List[(Char, Int)]): List[List[(Char, Int)]] = {
	  for {
	    sublist <- a;
	    itemB <- b
	  } yield (sublist ++ List[(Char,Int)](itemB))
	}
	
	def combHelper(acc: List[List[(Char, Int)]], curr: List[List[(Char, Int)]]): List[List[(Char, Int)]] = {
	  if (curr.isEmpty) acc else {
	    if (curr.tail.isEmpty) {
	      (acc ++ comb(acc, curr.head))
	    } else {
	      val temp = acc ++ comb(acc, curr.head)
	      combHelper(temp, curr.tail)
	    }
	  }
	}
	
	
    
    val f = combHelper(List[Occurrences](List()), p)
    
    f map (x => x.sortBy(_._1))
    
    
    
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xx = x.unzip._1
    val yy = y.unzip._1
    val xu = xx.toSet
    val yu = yy.toSet
    
    
    
    if (yu forall (z => xu.contains(z))){
      //x.filter(((a) => !yu.contains(a._1)))
      val changed = for{
        itemX <- x
        itemY <- y
        if (itemX._1 equals itemY._1)
        if (itemX._2   >    itemY._2)
      } yield (itemX._1, itemX._2 - itemY._2)
      
      (x.filter(((a) => !yu.contains(a._1))) ++ changed ).sorted 
      
    }else{
      x
    }
  }
  
  def getAllLegalWord(sentences: List[Sentence]): List[Word] = {
    val px = for {
      sentence <- sentences
      word <- sentence
      newWord <- wordAnagrams(word)
    } yield newWord
    
    px
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    
    if (sentence.isEmpty) List(List()) else {
    
    val so = sentenceOccurrences(sentence)
    val cso = combinations(so)
    val cso2 = cso.filter(x => wordAnagramsContains(x))
    val p = cso2.combinations(cso2.length).toList
    
    val p2 = for (list <- p) yield (list map wordFromDictionary)
    val px = getAllLegalWord(p2)
    
    def helperWords(list: List[Word], occ: Occurrences): List[List[Word]] = {
      if (occ.isEmpty) List() else {
        val tempLegal = combinations(occ).filter(x => wordAnagramsContains(x)) map wordFromDictionary
        val legal = getAllLegalWord(List(tempLegal))
        
        val n = for {
          newWord <- legal
        } yield list ++ List(newWord)
        
        /*println(list)
        println(legal)
        println("")*/
        
        n
      }
    }
    
    def helperSentences(list: List[Word], occ: Occurrences): List[List[Word]] = {
      
      def check (boo:Boolean, word: Word, left: Occurrences): List[List[Word]] = {
        if (boo)List(List(word)) else helperWords(List(word), left) 
      }
      
      if (occ.isEmpty) List(list) else {
        val tempLegal = combinations(occ).filter(x => wordAnagramsContains(x)) map wordFromDictionary
        
        val legal = getAllLegalWord(List(tempLegal))
        val combs = for {
          word <- legal
          left = subtract(occ, wordOccurrences(word))
          nothingLeft = left.isEmpty
          rest <- check(nothingLeft, word, occ)
        } yield (rest  ++ list)
        /*println(list + "  occ  " + occ)
        println(list + "  wor  " + list)
        println(list + "  leg  " + legal)
        println(list + "  com  " + combs)
        println("  ")*/
        
        combs
      }
      
      
    }
    
    
    val px3 = for {
      
      word <- px // all the words
       // a word
      rest <- helperSentences(List(word), subtract(so, wordOccurrences(word)))
    } yield (rest)
    
    lazy val p3 = for {
      n <- 1 to p2.length // iterate the list
      sentence <- p2 // all the words
      word <- sentence // a word
      rest <- helperSentences(List(word), subtract(so, wordOccurrences(word)))
    } yield (rest)
    
    println(px3.length + " " + px3)
       
     val p4 = px3.filter(x => sentenceOccurrences(x).toSet.equals(so.toSet)) 
     
     println(p4.length + "  p4 " + p4)
    
     p4.toList
  }
  }
}
