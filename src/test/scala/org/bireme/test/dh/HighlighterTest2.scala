package org.bireme.test.dh

import org.bireme.dh.{Config, Highlighter, Tools}
import org.scalatest.flatspec.AnyFlatSpec

class HighlighterTest2 extends AnyFlatSpec {
  val highlighter: Highlighter = new Highlighter("/home/javaapps/sbt-projects/DeCSHighlighter/decs/decs")

  "The highlighter" should "find 'matadouros' (PT) as descriptor" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=None, pubType=None,
                              scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "Os matadouros são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("matadouros"))
  }

  it should "find 'abattoirs' (EN) as descriptor" in {
    val conf: Config = Config(scanLang=Some("en"), outLang=None, pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("abattoirs"))
  }

  it should "not find 'abattoirs' (EN) as descriptor" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=None, pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.isEmpty)
  }

  it should "find 'matadouros' (PT) but not 'abattoirs' as descriptor" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=None, pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "Os matadouros e abattoirs são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("matadouros"))
  }

  it should "find 'abattoirs' (EN) as descriptor but translate it into 'mataderos'" in {
    val conf: Config = Config(scanLang=Some("en"), outLang=Some("es"), pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("mataderos"))
  }

  it should "find 'abattoirs' (EN) and 'matadouros' (PT) as descriptor but translate only the second into 'mataderos'" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=Some("es"), pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs and matadouros are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("mataderos"))
  }

  it should "find 'abattoirs' (EN) and 'matadouros' (PT) and 'abreviaturas como Assunto' (PT) as descriptor but translate " +
    "only the last two into 'mataderos' and 'abreviaturas como asunto'" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=Some("es"), pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs and matadouros and Abreviaturas como Assunto are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert {
      val terms = Set("mataderos", "abreviaturas como asunto")
      val seq3 = seq2.map(Tools.uniformString)
      terms.forall(seq3.contains)
    }
  }

  it should "find 'deformidades' (PT) and 'matadouros' (PT) and translate both into 'mataderos' and 'anomalias congenitas'" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=Some("es"), pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "As deformidades causadas pelos matadouros são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert {
      val terms = Set("mataderos", "anomalias congenitas")
      val seq3 = seq2.map(Tools.uniformString)
      terms.forall(seq3.contains)
    }
  }

  it should "find 'deformidades' (PT) and 'matadouros' (PT)  but translate " +
    "only the last one into 'mataderos'" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=Some("es"), pubType=None,
      scanDescriptors=true, scanSynonyms=false, onlyPreCod=false)
    val str = "As deformidades causadas pelos matadouros são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("mataderos"))
  }

  it should "find 'deformidades' (PT) and 'matadouros' (PT)  but translate " +
    "only the first one into 'anomalias congenitas'" in {
    val conf: Config = Config(scanLang=Some("pt"), outLang=Some("es"), pubType=None,
      scanDescriptors=false, scanSynonyms=true, onlyPreCod=false)
    val str = "As deformidades causadas pelos matadouros são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("anomalias congenitas"))
  }

  it should "find 'adulto' (PT) as descriptor. The document is precod." in {
    val conf: Config = Config(scanLang=Some("en"), outLang=Some("pt"), pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=true)
    val str = "The adult is cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("adulto"))
  }

  it should "find 'abattoirs' (EN) as descriptor because the document type is 'H' lowercase" in {
    val conf: Config = Config(scanLang=Some("en"), outLang=None, pubType=Some('h'),
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("abattoirs"))
  }

  it should "find 'abattoirs' (EN) as descriptor because the document type is 'H' uppercase" in {
    val conf: Config = Config(scanLang=Some("en"), outLang=None, pubType=Some('H'),
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head).equals("abattoirs"))
  }

  it should "find 'abattoirs' (EN) as descriptor but do not return anything because the document type is 'H' uppercase" in {
    val conf: Config = Config(scanLang=Some("en"), outLang=None, pubType=Some('T'),
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.isEmpty)
  }
}
