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
        val descr: String = term._1
        val id: String = term._2
        val firstChar: Char = descr.head
        map.get(firstChar) match {
          case Some(cseq) =>
            insertTerm(cseq, descr.tail, id)
            map
          case None =>
            val first: CharSeq = CharSeq(firstChar)
            insertTerm(first, descr.tail, id)
            map + (firstChar -> first)
        }
    }
  }

  /**
  * Insert a new descriptor/synonym into the graph
 *
    * @param seq the current graph
    * @param term the descriptor/synonym to be inserted
    * @param id the DeCS id of the descriptor/synonym to be inserted
    */
  @scala.annotation.tailrec
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
    * @return (the input text highlighted, Seq(initial position, final position, DeCS id, descriptor/synonym), Seq(descriptor/synonym))
    */
  def highlight(prefix: String,
                suffix: String,
                text: String,
                terms: Map[Char, CharSeq]): (String, Seq[(Int, Int, String, String)], Seq[String]) = {
    val (_, text2: String, seqPos: Seq[Int]) = Tools.uniformString2(text)
    val tags: Seq[(Int, Int)] = mergeTagsPos(findOpenTags(text2), findCloseTags(text2), findSelfCloseTags(text2))
    val seqElem = invertPos(tags, 0, text2.length)
    val (seq: Seq[(Int, Int, String, String)], set: Set[String]) = highlight(0, text2, terms, seqElem)

    // Adjust positions to the text with accents.
    val (marked: String, tend: Int) = seq.foldLeft[(String, Int)]("", 0) {
      case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, _: String, _: String)) =>
        val teBegin: Int = seqPos(termBegin)
        val teEnd: Int = seqPos(termEnd)
        val s: String = str + text.substring(lpos, teBegin) + prefix + text.substring(teBegin, teEnd + 1) + suffix
        (s, teEnd + 1)
    }
    val marked2 = if (tend >= text.length) marked else marked + text.substring(tend)
    val seq2 = seq.map(x => (seqPos(x._1), seqPos(x._2), x._3, x._4))

    (marked2, seq2, set.toSeq.sorted)
  }

  /**
    * Highlights all DeCS descriptors/synonyms of an input text
    * @param presu a prefix/suffix function that marks (put tags) the descriptor/synonym
    * @param text the input text to be highlighted
    * @param terms the graph of DeCS descriptors/synonyms
    * @return (the input text highlighted, Seq(initial position, final position, DeCS id, descriptor/synonym), Seq(descriptor/synonym))
    */
  def highlight(presu: String => String,
                text: String,
                terms: Map[Char, CharSeq]): (String, Seq[(Int, Int, String, String)], Seq[String]) = {
    val (_, text2: String, seqPos: Seq[Int]) = Tools.uniformString2(text)
    val tags: Seq[(Int, Int)] = mergeTagsPos(findOpenTags(text2), findCloseTags(text2), findSelfCloseTags(text2))
    val seqElem = invertPos(tags, 0, text2.length)
    val (seq: Seq[(Int, Int, String, String)], set: Set[String]) = highlight(0, text2, terms, seqElem)

    // Adjust positions to the text with accents.
    val (marked: String, tend: Int) = seq.foldLeft[(String, Int)]("", 0) {
      case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, _: String, _: String)) =>
        val teBegin: Int = seqPos(termBegin)
        val teEnd: Int = seqPos(termEnd)
        val s = str + text.substring(lpos, teBegin) + presu(text.substring(teBegin, teEnd + 1))
        (s, teEnd + 1)
    }
    val marked2 = if (tend >= text.length) marked else marked + text.substring(tend)
    val seq2 = seq.map(x => (seqPos(x._1), seqPos(x._2), x._3, x._4))

    (marked2, seq2, set.toSeq.sorted)
  }
  /**
  * Highlights all DeCS descriptors/synonyms of an input text
    * @param curPos current position inside input text
    * @param text input text (without accents)
    * @param terms the graph of DeCS descriptors/synonyms
    * @param seqElem sequence of (begin,end) positions of xml tags
    * @return (Seq(initial position, final position, DeCS id, descriptor/synonym), Set(descriptor/synonym))
    */
  private def highlight(curPos: Int,
                        text: String,
                        terms: Map[Char, CharSeq],
                        seqElem: Seq[(Int, Int)]): (Seq[(Int, Int, String, String)], Set[String]) = {
    val size = text.length

    findNextValidPos(curPos, seqElem) match {
      case Some((curPos2, range, seqElem2)) =>
        findValidTermStart(curPos2, text, range._2) match {
          case Some(curPos3) =>
            findTerm(curPos3, text, size, terms) match {
              case Some((endPos, id)) =>
                val term = text.substring(curPos3, endPos + 1)
                val (seq, set) = highlight(endPos + 1, text, terms, seqElem2)
                ((curPos3, endPos, id, term) +: seq, set + term)
              case None => highlight(curPos3 + 1, text, terms, seqElem2)
            }
          case None => highlight(range._2 + 1, text, terms, seqElem2)
        }

      case None => (Seq[(Int, Int, String, String)](), Set[String]())
    }
  }

  /**
    * Find the next valid position, given a seq of valid positions (seqElem)
    *
    * @param curPos current position
    * @param seqElem sequence of tuples (beginpos, endpos) of valid positions
    * @return (next valid position, the valid position range, original sequence less elements that are lesser than the next valid position)
    */
  @scala.annotation.tailrec
  private def findNextValidPos(curPos: Int,
                               seqElem: Seq[(Int, Int)]): Option[(Int, (Int, Int), Seq[(Int, Int)])] = {
    if (seqElem.isEmpty) None
    else {
      val head = seqElem.head
      if (curPos <= head._1) Some(head._1, seqElem.head, seqElem)
      else if (curPos <= head._2) Some(curPos, seqElem.head, seqElem)
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
      val ch: Char = text(curPos)
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
    * @param endPos last position of the input text
    * @return the next valid position in the input text to start a descriptor/synonym or None otherwise
    */
  private def findValidTermStart(curPos: Int,
                                 text: String,
                                 endPos: Int): Option[Int] = {
    if (curPos >= endPos) None
    else (curPos to endPos).find(x => isValidTermStart(x, text, endPos))
  }

  /**
    * @param curPos current position in the input text
    * @param text the input text
    * @param endPos last position of the input text
    * @return the current position if it is a valid position in the input text to start a descriptor/synonym or None otherwise
    */
  private def isValidTermStart(curPos: Int,
                               text: String,
                               endPos: Int): Boolean = {
    (curPos == 0) || ((curPos <= endPos) && !Tools.isLetterOrDigit(text(curPos - 1)) && Tools.isLetterOrDigit(text(curPos)))
  }

  /**
    * Finds the positions of open xml elements  <x d='ccccc'>
    * @param in input text
    * @return a sequence of subtexts in the form of
    *   (initial position of the open tag, end position of the open tag)
    */
  private def findOpenTags(in: String): Seq[(Int, Int)] = {
    val regex: Regex = "(<|&lt;) *[^ >/]{1,20}( +[a-zA-Z0-9]{1,10}=['\"][^'\"]{1,20}['\"])* *(>|&gt;)".r

    regex.findAllMatchIn(in).foldLeft(Seq[(Int,Int)]()) {
      case (seq: Seq[(Int, Int)], mat: Match) =>
        seq :+ (mat.start -> (mat.end - 1))
    }
  }

  /**
    * Finds the positions of close xml elements </x>
    * @param in input text
    * @return a sequence of subtexts in the form of
    *   (initial position of the close tag, end position of the close tag)
    */
  private def findCloseTags(in: String): Seq[(Int, Int)] = {
    val regex: Regex = "(<|&lt;) */ *[^ >]{1,20}( +[a-zA-Z0-9]{1,10}=['\"][^'\"]{1,20}['\"])* *(>|&gt;)".r

    regex.findAllMatchIn(in).foldLeft(Seq[(Int,Int)]()) {
      case (seq: Seq[(Int, Int)], mat: Match) =>
        seq :+ (mat.start -> (mat.end - 1))
    }
  }

  /**
    * Finds the positions of selfclose xml elements <x/>
    * @param in input text
    * @return a sequence of subtexts in the form of
    *   (initial position of the selfclose tag, end position of the selfclose tag)
    */
  private def findSelfCloseTags(in: String): Seq[(Int, Int)] = {
    val regex: Regex = "(<|&lt;) *[^ >/]{1,20}( +[a-zA-Z0-9]{1,10}=['\"][^'\"]{1,20}['\"])* */ *(>|&gt;)".r

    regex.findAllMatchIn(in).foldLeft(Seq[(Int,Int)]()) {
      case (seq: Seq[(Int, Int)], mat: Match) =>
        seq :+ (mat.start -> (mat.end - 1))
    }
  }

  /**
    *
    * @param openTags open tag (init, end) positions  <a>
    * @param closeTags close tag (init, end) positions  </a>
    * @param selfCloseTags open/close tag (init, end) positions  <a/>
    * @return a joinned sequence of (init, end) positions. Group and order
    */
  private def mergeTagsPos(openTags: Seq[(Int,Int)],
                           closeTags: Seq[(Int, Int)],
                           selfCloseTags: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    val pos1: Int = if (openTags.isEmpty) Int.MaxValue else openTags.head._1
    val pos2: Int = if (closeTags.isEmpty) Int.MaxValue else closeTags.head._1
    val pos3: Int = if (selfCloseTags.isEmpty) Int.MaxValue else selfCloseTags.head._1
    val pos: Int = Math.min(pos1, Math.min(pos2, pos3))

    if (pos == Int.MaxValue) Seq[(Int, Int)]()
    else if (pos == pos1) Seq((pos1, openTags.head._2)) ++ mergeTagsPos(openTags.tail, closeTags, selfCloseTags)
    else if (pos == pos2) Seq((pos2, closeTags.head._2)) ++ mergeTagsPos(openTags, closeTags.tail, selfCloseTags)
    else Seq((pos3, selfCloseTags.head._2)) ++ mergeTagsPos(openTags, closeTags, selfCloseTags.tail)
  }

  /**
    * Given an input text and the positions of open/close tags returns the portions of text that do not include the tags
    * @param tagPos positions of the begin and end of a tag
    * @param curPos current position in the text
    * @param strLength the input text length
    * @return the portions of text that do not contain tags
    */
  private def invertPos(tagPos: Seq[(Int, Int)],
                        curPos: Int,
                        strLength: Int): Seq[(Int, Int)] = {
    if (curPos < strLength) {
      if (tagPos.isEmpty) Seq((curPos, strLength - 1))
      else {
        val head = tagPos.head
        if (curPos < tagPos.head._1) Seq((curPos, head._1 - 1)) ++ invertPos(tagPos.tail, head._2 + 1, strLength)
        else invertPos(tagPos.tail, head._2 + 1, strLength)
      }
    } else Seq[(Int, Int)]()
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

  val highlighter = new Highlighter()
  val terms: Map[String,String] = Tools.decs2Set(decs)
  val tree: Map[Char, CharSeq] = highlighter.createTermTree(terms)
  val src: BufferedSource = Source.fromFile(inFile, encoding)
  val text: String = src.getLines().mkString("\n")
  val (marked: String, seq: Seq[(Int, Int, String, String)], set: Seq[String]) =
    highlighter.highlight(prefix, suffix, text, tree)

  if (seq.isEmpty) println("No descriptors found.")
  else {
    println("Descriptors found:")
    seq.foreach(tuple => println(s"(${tuple._1},${tuple._2}) - ${tuple._4}"))
    //println("Positions:")
    //seq.foreach(pos => println(pos))
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile), encoding))
    writer.write(marked)
    writer.close()
  }
  src.close()
}