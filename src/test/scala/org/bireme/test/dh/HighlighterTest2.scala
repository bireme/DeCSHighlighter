package org.bireme.test.dh

import org.bireme.dh.{Config, Highlighter, Tools}
import org.scalatest.flatspec.AnyFlatSpec

class HighlighterTest2 extends AnyFlatSpec {
  val highlighter: Highlighter = new Highlighter("/home/javaapps/sbt-projects/DeCSHighlighter/decs/decs")

  "The highlighter" should "find 'matadouros' (PT) as descriptor" in {
    val conf: Config = Config(Some("pt"), None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "Os matadouros são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")

    assert (Tools.uniformString(seq2.head._1).equals("matadouros"))
  }

  it should "find 'etiologia' (PT) as qualifier" in {
    val conf: Config = Config(Some("pt"), None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "Devemos tudo ao estudo /etiologia"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("etiologia"))
  }

  it should "find 'abattoirs' (EN) as descriptor" in {
    val conf: Config = Config(Some("en"), None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("abattoirs"))
  }

  it should "not find 'abattoirs' (EN) as descriptor" in {
    val conf: Config = Config(Some("pt"), None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.isEmpty)
  }

  it should "find 'matadouros' (PT) but not 'abattoirs' as descriptor" in {
    val conf: Config = Config(Some("pt"), None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "Os matadouros e abattoirs são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("matadouros"))
  }

  it should "find 'abattoirs' (EN) as descriptor but translate it into 'mataderos'" in {
    val conf: Config = Config(Some("en"), Some("es"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("mataderos"))
  }

  it should "find 'abattoirs' (EN) and 'matadouros' (PT) as descriptor but translate only the second into 'mataderos'" in {
    val conf: Config = Config(Some("pt"), Some("es"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "The abattoirs and matadouros are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("mataderos"))
  }

  it should "find 'abattoirs' (EN) and 'matadouros' (PT) and 'abreviaturas como Assunto' (PT) as descriptor but translate " +
    "only the last two into 'mataderos' and 'abreviaturas como asunto'" in {
    val conf: Config = Config(Some("pt"), Some("es"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "The abattoirs and matadouros and Abreviaturas como Assunto are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert {
      val terms = Set("mataderos", "abreviaturas como asunto")
      val seq3 = seq2.map(x => Tools.uniformString(x._1))
      terms.forall(seq3.contains)
    }
  }

  /*it should "find 'deformidades' (PT) and 'matadouros' (PT) and translate both into 'mataderos' and 'anomalias congenitas'" in {
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
  }*/

  it should "find 'deformidades' (PT) and 'matadouros' (PT)  but translate " +
    "only the last one into 'mataderos'" in {
    val conf: Config = Config(Some("pt"), Some("es"), scanMainHeadings=true, scanEntryTerms=false, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "As deformidades causadas pelos matadouros são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("mataderos"))
  }

  it should "find 'deformidades' (PT) and 'matadouros' (PT)  but how it doesnt what main heading do not show nothing " in {
    val conf: Config = Config(Some("pt"), Some("es"), scanMainHeadings=false, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    val str = "As deformidades causadas pelos matadouros são crueis!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.isEmpty)
  }

  it should "find 'adulto' (PT) as check tag." in {
    val conf: Config = Config(Some("en"), Some("pt"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)
    /*val conf: Config = Config(scanLang=Some("en"), outLang=Some("pt"), pubType=None,
      scanDescriptors=true, scanSynonyms=true, onlyPreCod=true)*/
    val str = "The adult is cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("adulto"))
  }

  it should "find 'abattoirs' (EN) as descriptor because the document type is 'H' lowercase" in {
    val conf: Config = Config(Some("en"), None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("abattoirs"))
  }

  it should "find 'abattoirs' (EN) as descriptor because the document type is 'H' uppercase" in {
    val conf: Config = Config(Some("en"), outLang=None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("abattoirs"))
  }

  it should "find 'abattoirs' (EN) as descriptor but do not return anything because the document type is 'H' uppercase" in {
    val conf: Config = Config(Some("en"), outLang=None, scanMainHeadings=false, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "The abattoirs are cruel!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.isEmpty)
  }

  it should "return 'Musa' the descriptor instead of 'Banana' the synonym" in {
    val conf: Config = Config(Some("pt"), outLang=None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "A banana está madura"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    val seq3: Seq[String] = seq2.map(_._1)

    assert (seq3.contains("Musa"))
  }

  it should "return 'Femmes' the descriptor instead of 'Meninas' the synonym" in {
    val conf: Config = Config(Some("pt"), outLang=Some("fr"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "As meninas no Brasil são lindas!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.contains("Femmes"))
  }

  it should "return 'Mulheres' the descriptor instead of 'Meninas' the synonym" in {
    val conf: Config = Config(Some("pt"), outLang=Some("pt"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "As meninas no Brasil são lindas!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.contains("Mulheres"))
  }

  it should "return 'Mulheres' the descriptor instead of 'Meninas' the synonym (no language configuration)" in {
    val conf: Config = Config(None, None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "As meninas no Brasil são lindas!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.contains("Mulheres"))
  }

  it should "not return 'Femmes' the descriptor instead of 'Meninas' the synonym because we do not want synonyms" in {
    val conf: Config = Config(Some("pt"), outLang=Some("fr"), scanMainHeadings=true, scanEntryTerms=false, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "As meninas no Brasil são lindas!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.isEmpty)
  }

  it should "return 'Brésil' as geographic" in {
    val conf: Config = Config(Some("pt"), outLang=Some("fr"), scanMainHeadings=true, scanEntryTerms=false, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=true)
    val str = "As meninas no Brasil são lindas!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    val seq3: Seq[String] = seq2.map(_._1)

    assert (seq3.contains("Brésil"))
  }

  it should "return 'Brasil' as geographic" in {
    val conf: Config = Config(Some("fr"), outLang=Some("pt"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=true)
    val str = "Les filles de la république fédérative du Brésil sont magnifiques!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (seq2.contains("Brasil"))
  }

  it should "return 'Mulheres' as descriptor and 'Brasil' as geographic" in {
    val conf: Config = Config(Some("fr"), outLang=Some("pt"), scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=false,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=true)
    val str = "La femme de la République fédérative du Brésil est magnifique!"

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert {
      val terms = Set("mulheres", "brasil")
      val seq3 = seq2.map(x => Tools.uniformString(x._1))
      terms.forall(seq3.contains)
    }
  }

  it should "find 'análise' (PT) as qualifier but not 'matadouros'" in {
    val conf: Config = Config(Some("pt"), Some("pt"), scanMainHeadings=false, scanEntryTerms=false, scanQualifiers=true,
      scanPublicationTypes=false, scanCheckTags=false, scanGeographics=false)
    val str = "Os matadouros no Brasil precisam de uma análise precisa de seu uso."

    val (text, seq, seq2) = highlighter.highlight("<em>", "</em>", str, conf)
    println(s"text=$text seq=$seq seq2=$seq2")
    assert (Tools.uniformString(seq2.head._1).equals("analise"))
  }
}
