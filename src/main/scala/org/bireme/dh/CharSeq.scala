/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import scala.collection.mutable

/**
  * Class to represent a DeCS descriptor character to compound the tree of descriptors, qualifiers and synonyms
  *
  * @param ch the character to represent
  * @param other the next characters
  * @param isLeaf if true that char is the last character of a descriptor, qualifier or synonym
  *
  * author: Heitor Barbieri
  * date: September - 2018
  */
case class CharSeq(ch: Char,
                   var other: mutable.Map[Char, CharSeq],
                   var isLeaf: Boolean)

object CharSeq {
  def apply(ch: Char): CharSeq = new CharSeq(ch, mutable.Map(), isLeaf = false)
}
