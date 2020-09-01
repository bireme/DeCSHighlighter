/*=========================================================================

    DecsHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.test.dh

import org.bireme.dh.{Config, Highlighter, Tools}
import org.scalatest.flatspec.AnyFlatSpec

class HighlighterTest extends AnyFlatSpec {
  val highlighter: Highlighter = new Highlighter("/home/javaapps/sbt-projects/DeCSHighlighter/decs/decs")
  val conf: Config = Config(None, None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
                            scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)

  "The highlighter" should "find 'temefos' as descriptor" in {
    val str = "temefos"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
     println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals(str))
  }

  it should "not mark 'ants' as a descriptor" in {
    val str = "wants"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq, seq2=$seq2")
    assert (seq2.isEmpty)
  }

  it should "mark 'ants' as a descriptor" in {
    val str = "My ants wants to eat"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq, seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("ants"))
  }

  it should "find 'temefos' as descriptor - case 2" in {
     val str = "Vem cá temefos vem cá!!!"

     val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
     println(s"text=$text seq=$seq seq2=$seq2")
     assert (Tools.uniformString(seq2.head).equals("temefos"))
   }

  it should "find 'temefos' as descriptor - case 3" in {
    val str = "Vem cá temefos, vem cá!!!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("temefos"))
  }

  it should "find 'temefos' and 'health'" in {
    val str = "Dr Temefos, wants to promote health in the USA."

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    val seq3 = seq2.map(x => Tools.uniformString(x))
    assert (seq3.contains("temefos") && seq3.contains("health") && seq3.contains("the"))
  }

  it should "find 'Abreviaturas como Assunto' as descriptor" in {
    val str = "'Abreviaturas como Assunto' é um descritor do Decs"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.map(x => Tools.uniformString(x)).contains("abreviaturas como assunto"))
  }

  it should "find 'Abreviaturas como Assunto' as descriptor - case 2" in {
    val str = "'abreviaturas como assunto' é um descritor do Decs"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.map(x => Tools.uniformString(x)).contains("abreviaturas como assunto"))
  }

  it should "not find 'Abreviaturas como Assunto' as descriptor" in {
    val str = "'Abreviaturas como AssuntoX' é um descritor do Decs"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (!seq2.map(x => Tools.uniformString(x)).contains("abreviaturas como assunto"))
  }

  it should "not mark 'ant' inside a tag but only outside" in {
    val str = "<h1 xxx='ant is an animal'>my ant is small</h1>My ant is one year old"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<h1 xxx='ant is an animal'>my <em>ant</em> is small</h1>My <em>ant</em> is one year old"))
  }

  it should "not mark 'ant' inside a tag but only outside 2" in {
    val str = "<h1 xxx='ant is an animal'/>My ant is one year old"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<h1 xxx='ant is an animal'/>My <em>ant</em> is one year old"))
  }

  it should "should mark 'ant' outside a tag" in {
    val str = "<h1>The ant is coming</h1>"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<h1><em>The</em> <em>ant</em> is coming</h1>"))
  }

  it should "mark 'ant' tag" in {
    val str = "The ant is coming"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<em>The</em> <em>ant</em> is coming"))
  }

  it should "mark 'off pump coronary artery bypass' and 'graft' terms" in {
    val str = "the ็off pump coronary artery bypass graft้ (OPCABG)"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("<em>the</em> <em>็off pump coronary artery bypass</em> <em>graft</em>้ (OPCABG)"))
  }

  it should "mark 'papers' and 'information' terms" in {
    val str = "We researched papers published on ‘의료정보’ and ‘medical information’ in various Korean journals during a 10-year period from 2005 to 2015."

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert(text.equals("We researched <em>papers</em> published on ‘의료정보’ and ‘medical <em>information</em>’ in various Korean journals during a 10-year period from 2005 to 2015."))
  }
}
