/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.file.Path

import org.apache.lucene.document.Document
import org.apache.lucene.index.{DirectoryReader, Term}
import org.apache.lucene.search.{IndexSearcher, MatchAllDocsQuery, Query, ScoreDoc, TermQuery, TopDocs}
import org.apache.lucene.store.{FSDirectory, MMapDirectory}

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/**
  * The configuration class used to filter retrieved documents
  * @param scanLang language to be used to find terms/synonyms. Valid values are: 'en', 'es', 'pt' and 'fr'
  * @param outLang language used to output terms/synonyms. Valid values are: 'en', 'es', 'pt' and 'fr'
  * @param scanMainHeadings indicate if main headings (descriptors) should be scanned.
  * @param scanEntryTerms indicate if entry terms (synonyms) should be scanned.
  * @param scanQualifiers indicate if qualifier terms should be scanned.
  * @param scanPublicationTypes indicate if publication types should be scanned.
  * @param scanCheckTags indicate if check tags (precodified terms) should be scanned.
  * @param scanGeographics indicate if geographic terms should be scanned.
  */
case class Config(
  scanLang: Option[String],
  outLang: Option[String],
  scanMainHeadings: Boolean,
  scanEntryTerms: Boolean,
  scanQualifiers: Boolean,
  scanPublicationTypes: Boolean,
  scanCheckTags: Boolean,
  scanGeographics: Boolean
)

/**
  * Class to highlight all DeCS descriptors and synonyms of an input text
  *
  * @param decsPath path to the DeCS' Lucene index
  * author: Heitor Barbieri
  * date: September - 2018
*/
class Highlighter(decsPath: String) {
  val indexPath: Path = new File(decsPath).toPath
  //val directory: FSDirectory = FSDirectory.open(indexPath)
  val directory: FSDirectory = new MMapDirectory(indexPath)
  val ireader: DirectoryReader = DirectoryReader.open(directory)
  val isearcher: IndexSearcher = new IndexSearcher(ireader)
  val terms: Map[Char, CharSeq] = createTermTree(decs2Map(isearcher))
  val langs = Set("en", "es", "pt", "fr")

  def close(): Unit = {
    ireader.close()
    directory.close()
  }

  /**
    * Create a map of descriptors, qualifiers and synonyms of the DeCS
    *
    * @param isearcher Lucene search object
    * @return a map of descriptor -> id
    */
  private def decs2Map(isearcher: IndexSearcher): Map[String,String] = {
    // descriptors that should be avoided because are common words and have other meanings in other languages
    //val stopwords = Set("la", "foram", "amp", "www")
    val stopwords = Set("amp", "www", "ano")
    val map: mutable.Map[String, String] = mutable.Map[String,String]()
    val query: Query = new MatchAllDocsQuery()

    getNextDoc(isearcher, query).foreach {
      doc =>
        val id: String = doc.get("id")
        val term: String = doc.get("term_normalized")
//println(s"id=$id term=$term")
        if (stopwords.contains(term)) map
        else map += (term -> id)
    }
    map.toMap
  }

  /**
    * Create a lazy list of documents retrieved by a search expression
    * @param isearcher Lucene search object
    * @param query search expression
    * @return a lazy list of the retrieved documents
    */
  private def getNextDoc(isearcher: IndexSearcher,
                         query: Query): LazyList[Document] = {
    getNextDoc(isearcher, query, None, 0)
  }

  private def getNextDoc(isearcher: IndexSearcher,
                         query: Query,
                         top: Option[TopDocs],
                         curIndex: Int): LazyList[Document] = {
    val curIndex2 = curIndex % 1000
    //println(s"getNextDoc curIndex=$curIndex curIndex2=$curIndex2")
    //val total: Long = top.map(_.totalHits.value).getOrElse(0)

    val total: Int = top.map(_.scoreDocs.length).getOrElse(0)
    val (index: Int, topDocs: TopDocs) = top match {
      case Some(topdocs) =>
        curIndex2 match {
          case cur if cur < (total - 1) => (cur, topdocs)
          case cur if total == 0 => (cur, topdocs)
          case _ => (0, loadTopDocs(isearcher, query, Some(topdocs.scoreDocs(total.toInt - 1))))
        }
      case None => (0, loadTopDocs(isearcher, query, None))
    }

    //if (topDocs.totalHits.value > 0) {
    if (topDocs.scoreDocs.length > 0) {
      isearcher.doc(topDocs.scoreDocs(index).doc) #:: getNextDoc(isearcher, query, Some(topDocs), curIndex + 1)
    } else LazyList.empty
  }

  private def loadTopDocs(isearcher: IndexSearcher,
                          query: Query,
                          last: Option[ScoreDoc]): TopDocs = {
     last match {
       case Some(lst) => isearcher.searchAfter(lst, query, 1000)
       case None => isearcher.search(query, 1000)
     }
  }

  /**
  * Given a list of DeCS terms/synonyms, create a graph with then where each letter of term is a node of the graph
    * @param terms a map of descriptor -> DeCS id
    * @return a map of initial letter of the descriptor -> a sequence of CharSeq
    */
  private def createTermTree(terms: Map[String,String]): Map[Char, CharSeq] = {
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
          val cseq: CharSeq = CharSeq(term.head)
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
    * @return (highlightedText, Seq(initial position, final position, DeCS id, descriptor, text descriptor), Set(descriptor))
    */
  def highlight(prefix: String,
                suffix: String,
                text: String,
                conf: Config): (String, Seq[(Int, Int, String, String, String)], Seq[String]) = {
    val (text2: String, seqPos: Seq[Int]) = Tools.uniformString2(text)
    val tags: Seq[(Int, Int)] = mergeTagsPos(findOpenTags(text2), findCloseTags(text2), findSelfCloseTags(text2))
    val seqElem: Seq[(Int, Int)] = invertPos(tags, 0, text2.length)
    val (seq: Seq[(Int, Int, String, String)], set: Set[String]) = highlight(0, text2, text2.length, seqElem, conf)

//println(s"text=[$text] seqElem=$seqElem seq=$seq set=$set")

    // Adjust positions to the text with accents.
    val (marked: String, tend: Int) = seq.foldLeft[(String, Int)]("", 0) {
      case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, _: String, _: String)) =>
        val teBegin: Int = seqPos(termBegin)
        val teEnd: Int = seqPos(termEnd)
        val s: String = str + text.substring(lpos, teBegin) + prefix + text.substring(teBegin, teEnd + 1) + suffix
        (s, teEnd + 1)
    }
    val marked2: String = if (tend >= text.length) marked else marked + text.substring(tend)
    val seq2: Seq[(Int, Int, String, String, String)] = seq.map {
      x =>
        val spos1: Int = seqPos(x._1)
        val spos2: Int = seqPos(x._2)
        (spos1, spos2, x._3, x._4, text.substring(spos1, spos2 + 1))
    }

    (marked2, seq2, set.toSeq.sorted)
  }

  /**
    * Highlights all DeCS descriptors/synonyms of an input text
    * @param presu a prefix/suffix function that marks (put tags) the descriptor/synonym
    * @param text the input text to be highlighted
    * @return (Seq(initial position, final position, DeCS id, descriptor, text descriptor), Set(descriptor))
    */
  def highlight(presu: String => String,
                text: String,
                conf: Config): (String, Seq[(Int, Int, String, String, String)], Seq[String]) = {
    val (text2: String, seqPos: Seq[Int]) = Tools.uniformString2(text)
    val tags: Seq[(Int, Int)] = mergeTagsPos(findOpenTags(text2), findCloseTags(text2), findSelfCloseTags(text2))
    val seqElem: Seq[(Int, Int)] = invertPos(tags, 0, text2.length)
    val (seq: Seq[(Int, Int, String, String)], set: Set[String]) = highlight(0, text2, text2.length, seqElem, conf)

    // Adjust positions to the text with accents.
    val (marked: String, tend: Int) = seq.foldLeft[(String, Int)]("", 0) {
      case ((str: String, lpos: Int), (termBegin: Int, termEnd: Int, _: String, _: String)) =>
        val teBegin: Int = seqPos(termBegin)
        val teEnd: Int = seqPos(termEnd)
        val s = str + text.substring(lpos, teBegin) + presu(text.substring(teBegin, teEnd + 1))
        (s, teEnd + 1)
    }
    val marked2: String = if (tend >= text.length) marked else marked + text.substring(tend)
    val seq2: Seq[(Int, Int, String, String, String)] = seq.map {
      x =>
        val spos1: Int = seqPos(x._1)
        val spos2: Int = seqPos(x._2)
        (spos1, spos2, x._3, x._4, text.substring(spos1, spos2 + 1))
    }

    (marked2, seq2, set.toSeq.sorted)
  }

  /**
  * Highlights all DeCS descriptors/synonyms of an input text
    * @param curPos current position inside input text
    * @param text input text (without accents)
    * @param length input text (without accents) length
    * @param seqElem sequence of (begin,end) positions of xml tags
    * @return (Seq(initial position, final position, DeCS id, descriptor), Set(descriptor))
    */
  private def highlight(curPos: Int,
                        text: String,
                        length: Int,
                        seqElem: Seq[(Int, Int)],
                        conf: Config): (Seq[(Int, Int, String, String)], Set[String]) = {
    findNextValidPos(curPos, seqElem) match {
      case Some((curPos2, range, seqElem2)) =>
        findValidTermStart(curPos2, text, range._2) match {
          case Some(curPos3) =>
            findTerm(curPos3, text, range._2) match {
              case Some((endPos, id)) =>
                val term: String = text.substring(curPos3, endPos + 1)
                getDesiredTerm(term, openDocument(id), conf) match {
                  case Some(doc) =>
                    val (seq, set) = highlight(endPos + 1, text, length, seqElem2, conf)
                    val outTerm: String = Option(doc.get("term")).getOrElse("")
                    val ret1: (Seq[(Int, Int, String, String)], Set[String]) =
                      ((curPos3, endPos, id, outTerm) +: seq, set + outTerm)
                    ret1
                  case None => highlight(endPos + 1, text, length, seqElem2, conf)
                }
              case None =>
                highlight(curPos3 + 1, text, length, seqElem2, conf)
            }
          case None => highlight(range._2 + 1, text, length, seqElem2, conf)
        }
      case None => (Seq[(Int, Int, String, String)](), Set[String]())
    }
  }

  /**
    * Find the next valid position (element of seqElem), given a seq of valid positions (seqElem)
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
      val head: (Int, Int) = seqElem.head
      if (curPos <= head._1) Some(head._1, seqElem.head, seqElem)
      else if (curPos <= head._2) Some(curPos, seqElem.head, seqElem)
      else findNextValidPos(head._2 + 1, seqElem.tail)
    }
  }

  /**
  * Locate the next descriptor/synonym in the input text
    * @param curPos current position in the input text
    * @param text the input text
    * @param endPos the last valid position of the input text
    * @return Some((final position of the descriptor, DeCS id) if some descriptor was found or None otherwise
    */
  private def findTerm(curPos: Int,
                       text: String,
                       endPos: Int): Option[(Int, String)] = {
    if (curPos >= endPos) None
    else terms.get(text(curPos)).flatMap( findTerm(curPos + 1, text, endPos, _) )
  }

  /**
    * Locate the next descriptor/synonym in the input text
    * @param curPos current position in the input text
    * @param text the input text
    * @param endPos the last valid position of the input text
    * @param seq the current position in the graph of DeCS descriptors/synonyms
    * @return Some((final position of the descriptor, DeCS id) if some descriptor was found or None otherwise
    */
  private def findTerm(curPos: Int,
                       text: String,
                       endPos: Int,
                       seq: CharSeq): Option[(Int, String)] = {
    if (curPos <= endPos) {
      val terms: mutable.Buffer[(Int, String)] = seq.other.flatMap {
        cs2: CharSeq =>
          //val curChar = text(curPos)
          if (text(curPos) == cs2.ch) {
            val y = findTerm(curPos + 1, text, endPos, cs2) match {
              case Some(term) => Some(term)
              case None =>
                if (cs2.other.exists(cs3 => cs3.ch == 0) &&
                  ((curPos == endPos) || (!Tools.isLetterOrDigit(text(curPos + 1))))) {
                  val x = Some((curPos, cs2.id.toString()))
                  x
                } else None
            }
            y
          } else None
      }
      terms.sortWith((x, y) => x._1 <= y._1).lastOption
    } else None
  }

  /**
    * Given a set of documents and a term's string, return one document that has the term and follow the given
    * restrictions (show descriptor, show synonyms, only precod)
    * @param term the term content (string)
    * @param docs the input set of documents to be filtered
    * @param conf the initial filter conditions
    * @return the document chosen from the input set
    */
  private def getDesiredTerm(term: String,
                             docs: Seq[Document],
                             conf: Config): Option[Document] = {
    if (docs.isEmpty) None
    else {
      val inLang: Option[String] = conf.scanLang.map(_.toLowerCase).filter(langs.contains)
      val termNorm: String = Tools.uniformString(term)
      val inDocs: Seq[Document] = docs.filter(doc => doc.get("term_normalized").equals(termNorm) && (inLang.isEmpty ||
        doc.get("lang").equals(inLang.get)))

      inDocs.headOption.flatMap {
        inDoc =>
          val inLang1: String = inLang.getOrElse(inDoc.get("lang"))
          val termType: String = inDoc.get("termType")
          if ((conf.scanMainHeadings     && termType.equals("mainHeading")) ||
              (conf.scanQualifiers       && termType.equals("qualifier")) ||
              (conf.scanPublicationTypes && termType.equals("pubType")) ||
              (conf.scanCheckTags        && termType.equals("checkTag")) ||
              (conf.scanGeographics      && termType.equals("geographic"))) {

            if (conf.scanEntryTerms || Option(inDoc.get("synonym")).isEmpty) {
              val outLang: String = conf.outLang.map(_.toLowerCase).filter(langs.contains).getOrElse(inLang1)

              docs.find(doc => doc.get("lang").equals(outLang) && Option(doc.get("synonym")).isEmpty)
            } else None
          } else None
      }
    }
  }

  /**
    * Retrieve all documents that have the 'id' field content with the same value of the parameter 'id'
    * @param id the value used to filter the documents
    * @return the retrieved set of documents
    */
  private def openDocument(id: String): Seq[Document] = {
    Try {
      val scoreDocs = isearcher.search(new TermQuery(new Term("id", id)), 100).scoreDocs
      scoreDocs.foldLeft(Seq[Document]()) {
        case (seq, score) => seq :+ isearcher.doc(score.doc)
      }
    } match {
      case Success(value) => value
      case Failure(_) => Seq[Document]()
    }
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
    else (curPos to endPos).find(isValidTermStart(_, text, endPos))
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
        val head: (Int, Int) = tagPos.head
        if (curPos < tagPos.head._1) Seq((curPos, head._1 - 1)) ++ invertPos(tagPos.tail, head._2 + 1, strLength)
        else invertPos(tagPos.tail, head._2 + 1, strLength)
      }
    } else Seq[(Int, Int)]()
  }
}

object HighlighterApp extends App {
  private def usage(): Unit = {
    System.err.println("Application to highlights all DeCS descriptors and synonyms present in an input file.\n")
    System.err.println("usage: HighlighterApp")
    System.err.println("\t\t-inFile=<inFile>         - input file to be highlighted")
    System.err.println("\t\t-outFile=<outFile>       - the output file with the highlighted text")
    System.err.println("\t\t-decs=<path>             - path to the Lucene DeCS index")
    System.err.println()
    System.err.println("\t\t[-scanLang=<lang>]       - language of DeCS terms to be used (pt,en,es,fr). Default is to use all")
    System.err.println("\t\t[-outLang=<lang>]        - language of DeCS terms to be used in the output. If absent will present terms as they are in the text")
    System.err.println("\t\t[-prefix=<prefix>]       - text to be placed before each term")
    System.err.println("\t\t[-suffix=<suffix>]       - text to be placed after each term")
    System.err.println("\t\t[-encoding=<encoding>]   - encoding of the input and output file")
    System.err.println("\t\t[--scanMainHeadings      - scan for DeCS main headings (descriptors)")
    System.err.println("\t\t[--scanEntryTerms        - scan for DeCS entry terms (synonyms)")
    System.err.println("\t\t[--scanQualifiers]       - scan for DeCS qualifiers terms")
    System.err.println("\t\t[--scanPublicationTypes] - scan for DeCS publication types")
    System.err.println("\t\t[--scanCheckTags]        - scan for DeCS check tags (precodified terms)")
    System.err.println("\t\t[--scanGeographics]      - scan for DeCS geographics")
    System.err.println()
    System.err.println("\t\t[-pubType=(h|q|t|c|g)]   - [DEPRECATED] alias to DeCS type + synonyms" +
      "\n\t\t\t'h' equals to --scanMainHeadings and --scanEntryTerms" +
      "\n\t\t\t'q' equals to --scanQualifiers and --scanEntryTerms" +
      "\n\t\t\t't' equals to --scanPublicationTypes and --scanEntryTerms" +
      "\n\t\t\t'c' equals to -scanCheckTags and --scanEntryTerms" +
      "\n\t\t\t'g' equals to --scanGeographics  and --scanEntryTerms")
    System.exit(1)
  }

  if (args.length < 3) usage()

  val parameters = args.foldLeft[Map[String,String]](Map()) {
    case (map,par) =>
      val split = par.split(" *= *", 2)
      if (split.size == 1) map + (split(0).substring(2) -> "")
      else map + ((split(0).substring(1), split(1)))
  }

  val inFile: String = parameters("inFile")
  val outFile: String = parameters("outFile")
  val decs: String = parameters("decs")
  val scanLang: Option[String] = parameters.get("scanLang")
  val outLang: Option[String] = parameters.get("outLang")
  val prefix: String = parameters.getOrElse("prefix", "<em>")
  val suffix: String = parameters.getOrElse("suffix", "</em>")
  val encoding: String = parameters.getOrElse("encoding", "utf-8")
  val pubType: Char = parameters.getOrElse("pubType", " ").toLowerCase.charAt(0)

  val scanMainHeadings: Boolean = parameters.contains("scanMainHeadings") || (pubType == 'h')
  val scanEntryTerms: Boolean = parameters.contains("scanEntryTerms") || (pubType != ' ')
  val scanQualifiers: Boolean = parameters.contains("scanQualifiers") || (pubType == 'q')
  val scanPublicationTypes: Boolean = parameters.contains("scanPublicationTypes") || (pubType == 't')
  val scanCheckTags: Boolean = parameters.contains("scanCheckTags") || (pubType == 'c')
  val scanGeographics: Boolean = parameters.contains("scanGeographics") || (pubType == 'g')
  val scanSome: Boolean =  scanMainHeadings || scanQualifiers || scanPublicationTypes || scanCheckTags || scanGeographics

  if (!scanSome) throw new IllegalArgumentException("too mach parameters selected")

  val conf: Config = Config(scanLang, outLang, scanMainHeadings, scanEntryTerms, scanQualifiers, scanPublicationTypes,
                  scanCheckTags, scanGeographics)

  val src: BufferedSource = Source.fromFile(inFile, encoding)
  val text: String = src.getLines().mkString("\n")
  src.close()
  val highlighter: Highlighter = new Highlighter(decs)
  val (marked: String, seq: Seq[(Int, Int, String, String, String)], set: Seq[String]) =
    highlighter.highlight(prefix, suffix, text, conf)

  if (seq.isEmpty) println("No descriptors found.")
  else {
    println("Descriptors found:")
    seq.foreach(tuple => println(s"(${tuple._1},${tuple._2}) - ${tuple._4}"))
    println("\nMarked text:")
    println(marked)
    //println("Positions:")
    //seq.foreach(pos => println(pos))
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile), encoding))
    writer.write(marked)
    writer.close()
  }

  highlighter.close()
}
