/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import scala.io.Source

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
    * @return (the input text highlighted, Seq(initial position, final position, DeCS id, descriptor/synonym), Seq(descriptor/synonym))
    */
  def highlight(prefix: String,
                suffix: String,
                text: String,
                terms: Map[Char, CharSeq]): (String, Seq[(Int, Int, String, String)], Seq[String]) = {
    val text2 = Tools.uniformString(text)
    val (seq: Seq[(Int, Int, String, String)], set: Set[String]) = highlight(0, text2, terms)
    val (marked: String, tend: Int) = seq.foldLeft("", 0) {
      case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, _: String, _: String)) =>
        val s = str + text.substring(lpos, termBegin) + prefix + text.substring(termBegin, termEnd + 1) + suffix
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
    * @return (Seq(initial position, final position, DeCS id, descriptor/synonym), Seq(descriptor/synonym))
    */
  private def highlight(curPos: Int,
                        text: String,
                        terms: Map[Char, CharSeq]): (Seq[(Int,Int, String, String)], Set[String]) = {
    val size = text.length

    if (curPos == size) (Seq(),Set())
    else {
//println(s"highlight:${text.substring(curPos)}")
      findTerm(curPos, text, size, terms) match {
        case Some((endPos, id)) =>
          val term = text.substring(curPos, endPos + 1)
          val (seq, set) = highlight(endPos + 1, text, terms)
          ((curPos, endPos, id, term) +: seq, set + term)
        case None =>
          findValidTermStart(curPos + 1, text, size) match {
            case Some(pos) => highlight(pos, text, terms)
            case None => (Seq(),Set())
          }
      }
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
    if (curPos == text.length) None
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
    if (curPos == size) {
      val x = getTermEnd(curPos, text, size, seq)
      //println("findTerm saindo. curPos == text.length result=" + x)
      x
    } else {
      val ch = text(curPos)
      //println(s"findTerm sub=${text.substring(curPos)} curPos=$curPos ch=$ch")
      seq.other.find(cs => cs.ch.equals(ch)) match {
        case Some(cseq: CharSeq) =>
          val x = findTerm(curPos + 1, text, size, cseq) orElse getTermEnd(curPos, text, size, seq)
          //println("findTerm saindo. curPos" + curPos + " result=" + x)
          x
        case None =>
          //println(s"findTerm saindo. Não tem nem a primeira letra: [$ch] curPos=$curPos")
          getTermEnd(curPos, text, size, seq)
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
    lazy val cond2 = !text(curPos).isLetterOrDigit
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
    (curPos until size).find(x => isValidTermStart(x, text, size))
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
    (curPos == 0) || ((curPos < size) && !text(curPos - 1).isLetterOrDigit && text(curPos).isLetterOrDigit)
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
      map + ((split(0).substring(1), split(1)))
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
  val src = Source.fromFile(inFile, encoding)
  val text = src.getLines().mkString("\n")
  val (marked, seq, set) = highlighter.highlight(prefix, suffix, text, tree)

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