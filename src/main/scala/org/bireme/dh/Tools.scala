/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/


package org.bireme.dh

import collection.JavaConverters._

import java.text.Normalizer
import java.text.Normalizer.Form

import bruma.master.{MasterFactory, Record}

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
    val mst = MasterFactory.getInstance(decsDir).open()

    val map: Map[String,String] = mst.iterator().asScala.foldLeft(Map[String,String]()) {
      case (map2, rec) =>
        if (rec.getStatus == Record.Status.ACTIVE) {
          val id = getField(rec, 99).head
          val map3 = Seq(1,2,3).foldLeft(map2) {
            case (mp, tag) =>
              getField(rec, tag).foldLeft(mp) {
                case (mp2, fld) => mp2 + (fld -> id)
              }
          }
          getSubfield(rec, 23).foldLeft(map3) {
            case (mp, subfld) => mp + (subfld -> id)
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
        val content = fld.getContent

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
        val s1 = fld.getTagSubfields('i').asScala.foldLeft(set) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
        val s2 = fld.getTagSubfields('e').asScala.foldLeft(s1) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
        val s3 = fld.getTagSubfields('p').asScala.foldLeft(s2) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
        fld.getTagSubfields('s').asScala.foldLeft(s3) {
          case (set1, sub) => set1 + uniformString(sub.getContent)
        }
    }.filter(_.length >= 3)
  }

  /**
    * Converts all input charactes into lower case without graphical accents
    *
    * @param in input string to be converted
    * @return the converted string
    */
  def uniformString(in: String): String = {
    require(in != null)

    val s1 = Normalizer.normalize(in.trim.toLowerCase(), Form.NFD)

    s1.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "")
  }
}
