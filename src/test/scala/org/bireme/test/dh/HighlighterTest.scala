/*=========================================================================

    DecsHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.test.dh

import org.bireme.dh._
import org.scalatest.flatspec.AnyFlatSpec

class HighlighterTest extends AnyFlatSpec {
  val highlighter = new Highlighter()
  val descriptors: Predef.Map[String,String] = Tools.decs2Set("/home/javaapps/sbt-projects/DeCSHighlighter/decs/decs")
  val map: Map[Char, CharSeq] = highlighter.createTermTree(descriptors)

  "The highlighter" should "find 'temefos' as descriptor" in {
    val str = "temefos"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
     println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.head.equals(str))
  }

  it should "not mark 'ants' as a descriptor" in {
    val str = "wants"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq, seq2=$seq2")
    assert (seq2.isEmpty)
  }

  it should "mark 'ants' as a descriptor" in {
    val str = "My ants wants to eat"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq, seq2=$seq2")
    assert (seq2.head.equals("ants"))
  }

  it should "find 'temefos' as descriptor - case 2" in {
     val str = "Vem cá temefos vem cá!!!"

     val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
     println(s"text=$text seq=$seq seq2=$seq2")
     assert (seq2.head.equals("temefos"))
   }

  it should "find 'temefos' as descriptor - case 3" in {
    val str = "Vem cá temefos, vem cá!!!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.head.equals("temefos"))
  }

  it should "find 'temefos' and 'health'" in {
    val str = "Dr temefos, wants to promote health in the USA."

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.contains("temefos") && seq2.contains("health"))
  }

  it should "find 'Abreviaturas como Assunto' as descriptor" in {
    val str = "'Abreviaturas como Assunto' é um descritor do Decs"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.contains("abreviaturas como assunto"))
  }

  it should "find 'Abreviaturas como Assunto' as descriptor - case 2" in {
    val str = "'abreviaturas como assunto' é um descritor do Decs"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.contains("abreviaturas como assunto"))
  }

  it should "not find 'Abreviaturas como Assunto' as descriptor" in {
    val str = "'Abreviaturas como AssuntoX' é um descritor do Decs"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (!seq2.contains("abreviaturas como assunto"))
  }

  it should "not mark 'ant' inside a tag but only outside" in {
    val str = "<h1 xxx='ant is an animal'>my ant is small</h1>My ant is one year old"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<h1 xxx='ant is an animal'>my <em>ant</em> is small</h1>My <em>ant</em> is one year old"))
  }

  it should "not mark 'ant' inside a tag but only outside 2" in {
    val str = "<h1 xxx='ant is an animal'/>My ant is one year old"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<h1 xxx='ant is an animal'/>My <em>ant</em> is one year old"))
  }

  it should "should mark 'ant' outside a tag" in {
    val str = "<h1>The ant is coming</h1>"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<h1>The <em>ant</em> is coming</h1>"))
  }

  it should "mark 'ant' tag" in {
    val str = "The ant is coming"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, map)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("The <em>ant</em> is coming"))
  }
}
