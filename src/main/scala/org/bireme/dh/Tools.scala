/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/


package org.bireme.dh

import scala.jdk.CollectionConverters._
import java.text.Normalizer
import java.text.Normalizer.Form

import bruma.master.{Master, MasterFactory, Record}

/**
  * A class of helping functions
  *
  * author: Heitor Barbieri
  * date: September - 2018
  **/
object Tools {
  /**
    * Create a set of descriptors, qualifiers and synonyms of the DeCS
    *
    * @param decsDir directory to the Isis DeCS database
    * @return a map of descriptor -> id
    */
  def decs2Set(decsDir: String): Map[String,String] = {
    // descriptors that should be avoided because:
    val stopwords = Set("la", "foram", "amp", "www") // are common words and have other meanings in other languages
                    //Set("methods", "metodos", "methodology", "metodologia") // appear as section names of medline document

    val mst: Master = MasterFactory.getInstance(decsDir).open()

    val map: Map[String,String] = mst.iterator().asScala.foldLeft(Map[String,String]()) {
      case (map2, rec) =>
        if (rec.getStatus == Record.Status.ACTIVE) {
          val id: String = getField(rec, 99).head
          val map3: Map[String, String] = Seq(1,2,3).foldLeft(map2) {
            case (mp, tag) =>
              getField(rec, tag).foldLeft(mp) {
                case (mp2, fld) =>
                  if (stopwords.contains(fld)) mp2
                  else mp2 + (fld -> id)
              }
          }
          val map4 = getSubfield(rec, 23).foldLeft(map3) {
            case (mp, subfld) =>
              if (stopwords.contains(subfld)) mp
              else mp + (subfld -> id)
          }
          getSubfield(rec, 50).foldLeft(map4) {
            case (mp, subfld) =>
              if (stopwords.contains(subfld)) mp
              else mp + (subfld -> id)
          }
        } else map2
    }

    mst.close()
    map
  }

  /**
    * Retrieve a record field content from an Isis database
    * @param rec the Isis database record object
    * @param fldTag the tag of the field to be retrieved
    * @return the field content
    */
  private def getField(rec: Record,
                       fldTag: Int): Set[String] = {
    rec.getFieldList(fldTag).asScala.foldLeft(Set[String]()) {
      case (set, fld) =>
        val content: String = fld.getContent

        if (content.length > 3)  // remove 'lã'
          set + uniformString(content)
        else set
    }
  }

  /**
    *  Retrieve the content of a record subfieldfields (i,e,p,s)
    * @param rec the Isis database record object
    * @param fldTag the tag of the field to be retrieved
    * @return the subfield content
    */
  private def getSubfield(rec: Record,
                          fldTag: Int): Set[String] = {
    rec.getFieldList(fldTag).asScala.foldLeft(Set[String]()) {
      case (set, fld) =>
        val s1: Set[String] = fld.getTagSubfields('i').asScala.foldLeft(set) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
        val s2: Set[String] = fld.getTagSubfields('e').asScala.foldLeft(s1) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
        val s3: Set[String] = fld.getTagSubfields('p').asScala.foldLeft(s2) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
        fld.getTagSubfields('s').asScala.foldLeft(s3) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
    }.filter(_.length >= 3)
  }

  /**
    * Convert the input string characters into lower case without graphical accents (diacriticals)
    *
    * @param in input string to be converted
    * @return the converted string
    */
  def uniformString(in: String): String = {
    require(in != null)

    val s1: String = Normalizer.normalize(in.toLowerCase(), Form.NFD)

    //s1.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "")
    s1.replaceAll("([a-z])[\\p{InCombiningDiacriticalMarks}]", "$1")
  }

  /**
    * Convert the input string characters into lower case without graphical accents (diacriticals)
    *
    * @param in input string to be converted
    * @return (the converted string,
    *         an array where the index is the position of the character in the normalized string and its content is
    *         the position of the character in the original string)
    */
  def uniformString2(in: String): (String, Seq[Int]) = {
    require(in != null)

    val inlc: String = in.toLowerCase()
    val noOL: Seq[(Int, Int)] = getNoOtherLetter(inlc)
    val str: String = removeDiacriticals(inlc, 0, inlc.length, noOL)
    val arr: Array[Int] = getOriginalStrPos(inlc, str)

    (str, arr.toIndexedSeq)
  }

  /*
  def uniformString2(in: String): (String, String, Seq[Int]) = {
    require(in != null)

    val inlc: String = in.toLowerCase()
    val s1: String = Normalizer.normalize(inlc, Form.NFD)

    //val s2: String = s1.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "")
    val s2: String = s1.replaceAll("[\\p{InCombiningDiacriticalMarks}\\p{M}]", "")
    //val s2 = s1.replaceAll("([a-z])[\\p{InCombiningDiacriticalMarks}]", "$1")
    print("inlc=> ")
    inlc.foreach(ch=> print(s"$ch(${ch.toInt}) "))
    println()
    print("s1=> ")
    s1.foreach(ch=> print(s"$ch(${ch.toInt}) "))
    println()
    print("s2=> ")
    s2.foreach(ch=> print(s"$ch(${ch.toInt}) "))
    println()

    val arr: Array[Int] = getOriginalStrPos(inlc, s2)

    (s1, s2, arr.toIndexedSeq)
  }
   */

  /**
    * @param in input string
    * @return a sequence of intervals (beginPos, endPos) of characters that are not other letters (characters that
    *         are not usual letters (A-Z,a-z,0-9,accents,usual symbols), are for example characters of china, russian,
    *         etc alphabet
    */
  private def getNoOtherLetter(in: String): Seq[(Int,Int)] =
    getNoOtherLetter(in, 0, 0, in.length, Seq[(Int,Int)]())

  /**
    *
    * @param in input string
    * @param beginPos initial position of the input string of the next interval
    * @param curPos current position of the input string
    * @param length input string length
    * @param auxSeq auxiliary seq
    * @return a sequence of intervals (beginPos, endPos) of characters that are not other letters (characters that
    *         are not usual letters (A-Z,a-z,0-9,accents,usual symbols), are for example characters of china, russian,
    *         etc alphabet
    */
  @scala.annotation.tailrec
  private def getNoOtherLetter(in: String,
                               beginPos: Int,
                               curPos: Int,
                               length: Int,
                               auxSeq: Seq[(Int,Int)]): Seq[(Int,Int)] = {
    if (length == 0) Seq[(Int,Int)]()
    else if (curPos == length) {
      if (beginPos < curPos) auxSeq :+ (beginPos, curPos - 1)
      else auxSeq
    } else {
      val ch = in(curPos)
      if (ch.getType == Character.OTHER_LETTER) {
        if (beginPos == curPos) getNoOtherLetter(in, beginPos + 1, curPos + 1, length, auxSeq)
        else getNoOtherLetter(in, curPos + 1, curPos + 1, length, auxSeq :+ (beginPos, curPos - 1))
      } else {
        getNoOtherLetter(in, beginPos, curPos + 1, length, auxSeq)
      }
    }
  }

  /**
    * Remove diacriticals from an input string
    * @param text the input string
    * @param curPos current position of the input string
    * @param length size of the input string
    * @param noOtherLetter sequence of positions (beginPos, endPos) of the input string to have diacriticals removed
    * @return the input string with diacriticals removed
    */
  private def removeDiacriticals(text: String,
                                 curPos: Int,
                                 length: Int,
                                 noOtherLetter: Seq[(Int,Int)]): String = {
    if (curPos >= length) ""
    else {
      noOtherLetter.headOption match {
        case Some(head) =>
          val begin = head._1
          if (curPos < begin) {
            text.substring(curPos, begin) + removeDiacriticals(text, begin, length, noOtherLetter)
          } else {
            val end = head._2
            val s1 = text.substring(curPos, end + 1)
            val s2: String = Normalizer.normalize(s1, Form.NFD)
            val s3: String = s2.replaceAll("[\\p{InCombiningDiacriticalMarks}\\p{M}]", "")
            s3 + removeDiacriticals(text, end + 1, length, noOtherLetter.tail)
          }
        case None => text.substring(curPos)
      }
    }
  }

  /**
    * Given an string and its normalized (without accents) version, returns the position of each normalized
    * character in the original string
    *
    * @param originalStr the original string
    * @param transformedStr the normalized string
    * @return an array where the index is the position of the character in the normalized string and its content is
    *         the position of the character in the original string
    */
  def getOriginalStrPos(originalStr: String,
                        transformedStr: String): Array[Int]= {
    getOriginalStrPos(originalStr, 0, transformedStr, 0, Array.fill[Int](transformedStr.length)(0))
  }

  /**
    * Given an string and its normalized (without accents) version, returns the position of each normalized
    * character in the original string
    *
    * @param originalStr the original string
    * @param originalStrPos the current position of the original string
    * @param transformedStr the normalized string
    * @param transformedStrPos  the current postion of the normalized string
    * @param auxArr a temporary sequence of positions
    * @return an array where the index is the position of the character in the normalized string and its content is
    *         the position of the character in the original string
    */
  @scala.annotation.tailrec
  private def getOriginalStrPos(originalStr: String,
                                originalStrPos: Int,
                                transformedStr: String,
                                transformedStrPos: Int,
                                auxArr: Array[Int]): Array[Int] = {
    if ((originalStrPos == originalStr.length) || (transformedStrPos == transformedStr.length)) {
      auxArr
    } else if (originalStr(originalStrPos).getType == Character.NON_SPACING_MARK) {  // Graphical accent
      if (auxArr(transformedStrPos) == 0) auxArr(transformedStrPos) = originalStrPos
      getOriginalStrPos(originalStr,
                        originalStrPos + 1,
                        transformedStr,
                        transformedStrPos,
                        auxArr)
    } else {
      //auxArr(transformedStrPos) = originalStrPos
      if (auxArr(transformedStrPos) == 0) auxArr(transformedStrPos) = originalStrPos
      getOriginalStrPos(originalStr,
                        originalStrPos + 1,
                        transformedStr,
                        transformedStrPos + 1,
                        auxArr)
    }
  }

  /**
    * Check if the character is a small letter ('a' <= ch <= 'z') or a digit ('0' <= ch <= '9')
    * @param ch input character
    * @return true if it is a small letter or digit and false otherwise
    */
  def isLetterOrDigit(ch: Char): Boolean = {
    ((ch >= 'a') && (ch <= 'z')) || ((ch >= '0') && (ch <= '9'))
  }
}
