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
  * @param id the DeCS record identifier if the current positions represents a DeCS descriptor, otherwise an empty string
  *
  * author: Heitor Barbieri
  * date: September - 2018
  */
case class CharSeq(ch: Char,
                   other: mutable.Buffer[CharSeq],   // Changed from Set because Memory overflow error
                   id: StringBuilder)
object CharSeq {
  def apply(ch: Char): CharSeq = new CharSeq(ch, mutable.Buffer[CharSeq](), new mutable.StringBuilder())
}
