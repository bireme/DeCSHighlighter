/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/**
  * Class to highlight all DeCS descriptors and synonyms of an input text
  *
  * author: Heitor Barbieri
  * date: September - 2018
*/
class Highlighter {
  /**
  * Given a list of DeCS terms/synonyms, create a graph with then where each letter of term is a node of the graph
    * @param terms a map of descriptor -> DeCS id
    * @return a map of initial letter of the descriptor -> a sequence of CharSeq
    */
  def createTermTree(terms: Map[String,String]): Map[Char, CharSeq] = {
    terms.foldLeft(Map[Char, CharSeq]()) {
      case(map, term) =>
        val descr = term._1
        val id = term._2
        val firstChar = descr.head
        map.get(firstChar) match {
          case Some(cseq) =>
            insertTerm(cseq, descr.tail, id)
            map
          case None =>
            val first = CharSeq(firstChar)
            insertTerm(first, descr.tail, id)
            map + (firstChar -> first)
        }
    }
  }

  /**
  * Insert a new descriptor/synonym into the graph
    * @param seq the current graph
    * @param term the descriptor/synonym to be inserted
    * @param id the DeCS id of the descriptor/synonym to be inserted
    */
  private def insertTerm(seq: CharSeq,
                         term: String,
                         id: String): Unit = {
    if (term.isEmpty) {  // end of stream
      seq.other += CharSeq(0)
      seq.id.append(id)
    } else {
      seq.other.find(cs => cs.ch.equals(term.head)) match {
        case Some(cseq) => insertTerm(cseq, term.tail, id)
        case None =>
          val cseq = CharSeq(term.head)
          seq.other += cseq
          insertTerm(cseq, term.tail, id)
      }
    }
  }

  /**
  * Highlights all DeCS descriptors/synonyms of an input text
    * @param prefix a prefix to be placed before a found descriptor/synonym
    * @param suffix a suffix to be placed after a found descriptor/synonym
    * @param text the input text to be highlighted
    * @param terms the graph of DeCS descriptors/synonyms
    * @param skipXmlElem don't highlight texto inside xml tags neither between them
    * @return (the input text highlighted, Seq(initial position, final position, DeCS id, descriptor/synonym), Seq(descriptor/synonym))
    */
  def highlight(prefix: String,
                suffix: String,
                text: String,
                terms: Map[Char, CharSeq],
                skipXmlElem: Boolean): (String, Seq[(Int, Int, String, String)], Seq[String]) = {
    //println(s"[[$text]]")
    val text2 = Tools.uniformString(text)
    val seqElem: Seq[(Int, Int)] =
      if (skipXmlElem) {
        val openClose = findOpenCloseTags(text)
        val autoClose = findAutoCloseTag(text)
        mergeTagPositions(openClose, autoClose, Seq[(Int, Int)]())
      }
      else Seq[(Int, Int)]()
    val (seq: Seq[(Int, Int, String, String)], set: Set[String]) = highlight(0, text2, terms, seqElem)
    //println(s"seq=$seq")
    val (marked: String, tend: Int) = seq.foldLeft("", 0) {
      case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, _: String, _: String)) =>
        //println(s"lpos=$lpos termBegin=$termBegin termEnd=$termEnd str=$str\n\n")
        val s = str + text.substring(lpos, termBegin) + prefix + text.substring(termBegin, termEnd + 1) + suffix
        (s, termEnd + 1)
    }
    val marked2 = if (tend >= text.length) marked else marked + text.substring(tend)

    (marked2, seq, set.toSeq.sorted)
  }

  /**
    * Highlights all DeCS descriptors/synonyms of an input text
    * @param presu a prefix/suffix function that marks (put tags) the descriptor/synonym
    * @param text the input text to be highlighted
    * @param terms the graph of DeCS descriptors/synonyms
    * @param skipXmlElem do not highlight text inside xml tags neither between them
    * @return (the input text highlighted, Seq(initial position, final position, DeCS id, descriptor/synonym), Seq(descriptor/synonym))
    */
  def highlight(presu: String => String,
                text: String,
                terms: Map[Char, CharSeq],
                skipXmlElem: Boolean): (String, Seq[(Int, Int, String, String)], Seq[String]) = {
    val text2 = Tools.uniformString(text)
    val seqElem: Seq[(Int, Int)] =
      if (skipXmlElem) {
        val openClose = findOpenCloseTags(text)
        val autoClose = findAutoCloseTag(text)
        mergeTagPositions(openClose, autoClose, Seq[(Int, Int)]())
      } else Seq[(Int, Int)]()
    val (seq: Seq[(Int, Int, String, String)], set: Set[String]) = highlight(0, text2, terms, seqElem)
    val (marked: String, tend: Int) = seq.foldLeft("", 0) {
      case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, _: String, _: String)) =>
        val s = str + text.substring(lpos, termBegin) + presu(text.substring(termBegin, termEnd + 1))
        (s, termEnd + 1)
    }
    val marked2 = if (tend >= text.length) marked else marked + text.substring(tend)

    (marked2, seq, set.toSeq.sorted)
  }

  /**
  * Highlights all DeCS descriptors/synonyms of an input text
    * @param curPos current position inside input text
    * @param text input text
    * @param terms the graph of DeCS descriptors/synonyms
    * @param seqElem sequence of (begin,end) positions of xml tags
    * @return (Seq(initial position, final position, DeCS id, descriptor/synonym), Set(descriptor/synonym))
    */
  private def highlight(curPos: Int,
                        text: String,
                        terms: Map[Char, CharSeq],
                        seqElem: Seq[(Int, Int)]): (Seq[(Int, Int, String, String)], Set[String]) = {
    val size = text.length

    if (curPos == size) (Seq(), Set())
    else {
//println(s"highlight:${text.substring(curPos)}")
      val (curPos2, seqElem2) = findNextValidPos(curPos, seqElem)
      findTerm(curPos2, text, size, terms) match {
        case Some((endPos, id)) =>
          val term = text.substring(curPos2, endPos + 1)
          val (seq, set) = highlight(endPos + 1, text, terms, seqElem2)
          ((curPos2, endPos, id, term) +: seq, set + term)
        case None =>
          val (curPos3, seqElem3) = findNextValidPos(curPos2 + 1, seqElem2)
          findValidTermStart(curPos3, text, size) match {
            case Some(pos) => highlight(pos, text, terms, seqElem3)
            case None      => (Seq(), Set())
          }
      }
    }
  }

  /**
    * Find the next valid position, ie, a position that does not belong to the tuples (beginpos, endpos)
    * @param curPos current position
    * @param seqElem sequence of tuples (beginpos, endpos)
    * @return (next valid position, original sequence less elements that are lesser than the next valid position)
    */
  private def findNextValidPos(curPos: Int,
                               seqElem: Seq[(Int, Int)]): (Int, Seq[(Int, Int)]) = {
    if (seqElem.isEmpty) (curPos, seqElem)
    else {
      val head = seqElem.head
      if (curPos < head._1) (curPos, seqElem)
      else findNextValidPos(head._2 + 1, seqElem.tail)
    }
  }

  /**
  * Locate the next descriptor/synonym in the input text
    * @param curPos current position in the input text
    * @param text the input text
    * @param size the length of the input text
    * @param terms the graph of DeCS descriptors/synonyms
    * @return Some((final position of the descriptor, DeCS id) if some descriptor was found or None otherwise
    */
  private def findTerm(curPos: Int,
                       text: String,
                       size: Int,
                       terms: Map[Char, CharSeq]): Option[(Int, String)] = {
    if (curPos >= text.length) None
    else terms.get(text(curPos)).flatMap((cseq: CharSeq) => findTerm(curPos + 1, text, size, cseq))
  }

  /**
    * Locate the next descriptor/synonym in the input text
    * @param curPos current position in the input text
    * @param text the input text
    * @param size the length of the input text
    * @param seq the current position in the graph of DeCS descriptors/synonyms
    * @return Some((final position of the descriptor, DeCS id) if some descriptor was found or None otherwise
    */
  private def findTerm(curPos: Int,
                       text: String,
                       size: Int,
                       seq: CharSeq): Option[(Int, String)] = {
    if (curPos >= size) {
      getTermEnd(curPos, text, size, seq)
    } else {
      val ch = text(curPos)
      //println(s"findTerm sub=${text.substring(curPos)} curPos=$curPos ch=$ch")
      seq.other.find(cs => cs.ch.equals(ch)) match {
        case Some(cseq: CharSeq) => findTerm(curPos + 1, text, size, cseq) orElse getTermEnd(curPos, text, size, seq)
        case None => getTermEnd(curPos, text, size, seq)
      }
    }
  }

  /**
  * Locate the position of the last character of the descriptor/synonym in the input text
    * @param curPos current position in the input text
    * @param text the input text
    * @param size the length of the input text
    * @param seq the current position in the graph of DeCS descriptors/synonyms
    * @return Some((final position of the descriptor, DeCS id) if some descriptor was found or None otherwise
    */
  private def getTermEnd(curPos: Int,
                         text: String,
                         size: Int,
                         seq: CharSeq): Option[(Int, String)] = {
    val cond1 = curPos == size
    lazy val cond2 = !Tools.isLetterOrDigit(text(curPos))
    lazy val cond3 = seq.other.exists(cs => cs.ch.equals(0.toChar))
    val bool = (cond1 || cond2) && cond3

    if (bool) Some((curPos - 1, seq.id.toString)) else None
  }

  /**
    * @param curPos current position in the input text
    * @param text the input text
    * @param size the length of the input text
    * @return the next valid position in the input text to start a descriptor/synonym or None otherwise
    */
  private def findValidTermStart(curPos: Int,
                                 text: String,
                                 size: Int): Option[Int] = {
    if (curPos >= size) None
    else (curPos until size).find(x => isValidTermStart(x, text, size))
  }

  /**
    * @param curPos current position in the input text
    * @param text the input text
    * @param size the length of the input text
    * @return the current position if it is a valid position in the input text to start a descriptor/synonym or None otherwise
    */
  private def isValidTermStart(curPos: Int,
                               text: String,
                               size: Int): Boolean =
    (curPos == 0) || ((curPos < size) && !Tools.isLetterOrDigit(text(curPos - 1)) && Tools.isLetterOrDigit(text(curPos)))

  /**
    * Create a sequence of ordered tuples composed by two sequences
    * @param openClose first sequence of positions(begin, end)
    * @param autoClose second sequence of positions(begin, end)
    * @param auxSeq auxiliary sequence of positions(begin, end)
    * @return an ordered sequence of tuples composed by two sequences
    */
  def mergeTagPositions(openClose: Seq[(Int, Int)],
                        autoClose: Seq[(Int, Int)],
                        auxSeq:    Seq[(Int, Int)]): Seq[(Int, Int)] = {
    if (openClose.isEmpty) auxSeq ++ autoClose
    else if (autoClose.isEmpty) auxSeq ++ openClose
    else {
      val openHead: (Int, Int) = openClose.head
      val autoHead: (Int, Int) = autoClose.head
      if (openHead._1 > autoHead._1) mergeTagPositions(openClose.tail, autoClose, auxSeq :+ openHead)
      else mergeTagPositions(openClose, autoClose.tail, auxSeq :+ autoHead)
    }
  }

  /**
    * Finds all positions of xml elements  <x d='ccccc'>text</x>
    * @param in input text
    * @return a sequence of subtexts in the form of
    *   (initial position of the open tag, end position of the open  tag),
    *   (initial position of the close tag, end position of the close tag)
    */
  private def findOpenCloseTags(in: String): Seq[(Int, Int)] = {
    val regex: Regex = "< *([^ >]{1,20})( +[a-zA-Z0-9]{1,10}=['\"][^'\"]{1,20}['\"])* *>([^<]*)< */ *\\1 *>".r
    val regex2: Regex = "&lt; *([^ &]{1,20})( +[a-zA-Z0-9]{1,10}=['\"][^'\"]{1,20}['\"])* *&gt;.*?&lt; */ *\\1 *&gt;".r

    val iter: Iterator[Match] = regex.findAllMatchIn(in)
    if (iter.isEmpty) {
      val iter2: Iterator[Match] = regex2.findAllMatchIn(in)
      if (iter2.isEmpty) Seq[(Int,Int)]()
      else {
        iter2.foldLeft(Seq[(Int,Int)]()) {
          case (seq: Seq[(Int, Int)], mat: Match) => seq :+ (mat.start -> (mat.end - 1))
        }
      }
    } else {
      iter.foldLeft(Seq[(Int,Int)]()) {
        case (seq: Seq[(Int, Int)], mat: Match) => seq :+ (mat.start -> (mat.end - 1))
      }
    }
  }

  /**
    * Finds all positions of xml elements  <x d='ccccc'>text/>
    * @param in input text
    * @return a sequence of subtexts in the form of (initial position of the tag, end position of the tag)
    */
  private def findAutoCloseTag(in: String): Seq[(Int, Int)] = {
    val regex: Regex = "< *([^ /]{1,20})( +[a-zA-Z0-9]{1,10}=['\"][^'\"]{1,20}['\"])* */ *>".r
    val regex2: Regex = "&lt; *([^ /]{1,20})( +[a-zA-Z0-9]{1,10}=['\"][^'\"]{1,20}['\"])* */ *&gt;".r

    val iter: Iterator[Match] = regex.findAllMatchIn(in)
    if (iter.isEmpty) {
      val iter2: Iterator[Match] = regex2.findAllMatchIn(in)
      if (iter2.isEmpty) Seq[(Int,Int)]()
      else {
        iter2.foldLeft(Seq[(Int,Int)]()) {
          case (seq: Seq[(Int, Int)], mat: Match) => seq :+ (mat.start -> (mat.end - 1))
        }
      }
    } else {
      iter.foldLeft(Seq[(Int,Int)]()) {
        case (seq: Seq[(Int, Int)], mat: Match) => seq :+ (mat.start -> (mat.end - 1))
      }
    }
  }
}

object Highlighter extends App {
  private def usage(): Unit = {
    System.err.println("Application to highlights all DeCS descriptors and synonyms present in an input file.\n")
    System.err.println("usage: Highlighter")
    System.err.println("\t\t-inFile=<inFile>       - input file to be highlighted")
    System.err.println("\t\t-outFile=<outFile>     - the output file with the highlighted text")
    System.err.println("\t\t-decs=<path>           - path to the Isis database with the DeCS")
    System.err.println("\t\t[-prefix=<prefix>]     - text to be placed before each descriptor/synonym")
    System.err.println("\t\t[-suffix=<suffix>]     - text to be placed after  each descriptor/synonym")
    System.err.println("\t\t[-encoding=<encoding>] - encoding of the input and output file")
    System.err.println("\t\t[--skipXmlElem]        - if present, do not highlight text inside xml tags neither between them")
    System.exit(1)
  }

  if (args.length < 3) usage()

  val parameters = args.foldLeft[Map[String,String]](Map()) {
    case (map,par) =>
      val split = par.split(" *= *", 2)
      if (split.size == 1) map + (split(0).substring(2) -> "")
      else map + ((split(0).substring(1), split(1)))
  }

  val inFile = parameters("inFile")
  val outFile = parameters("outFile")
  val decs = parameters("decs")
  val prefix = parameters.getOrElse("prefix", "<em>")
  val suffix = parameters.getOrElse("suffix", "</em>")
  val encoding = parameters.getOrElse("encoding", "utf-8")
  val skipXmlElem = parameters.contains("skipXmlElem")

  val highlighter = new Highlighter()
  val terms: Map[String,String] = Tools.decs2Set(decs)
  val tree: Map[Char, CharSeq] = highlighter.createTermTree(terms)
  val src: BufferedSource = Source.fromFile(inFile, encoding)
  val text: String = src.getLines().mkString("\n")
  val (marked: String, seq: Seq[(Int, Int, String, String)], set: Seq[String]) =
    highlighter.highlight(prefix, suffix, text, tree, skipXmlElem)

  if (set.isEmpty) println("No descriptors found.")
  else {
    println("Descriptors found:")
    set.foreach(des => println(des))
    //println("Positions:")
    //seq.foreach(pos => println(pos))
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile), encoding))
    writer.write(marked)
    writer.close()
  }
  src.close()
}