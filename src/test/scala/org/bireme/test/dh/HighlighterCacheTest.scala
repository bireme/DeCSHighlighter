/*=========================================================================

    DecsHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.test.dh

import org.apache.lucene.index.{DirectoryReader, MultiBits}
import org.apache.lucene.store.MMapDirectory
import org.bireme.dh.{CharSeq, Config, Highlighter}
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.file.Paths
import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
  * Tests of the term tree built at the Highlighter creation and of the cache of the found terms
  */
class HighlighterCacheTest extends AnyFlatSpec {
  private val decsPath: String = "/home/javaapps/sbt-projects/DeCSHighlighter/decs/decs"
  private val highlighter: Highlighter = new Highlighter(decsPath)

  private def config(lang: Option[String]): Config =
    Config(lang, None, scanMainHeadings=true, scanEntryTerms=true, scanQualifiers=true,
           scanPublicationTypes=true, scanCheckTags=true, scanGeographics=true)

  /** @return all the terms of the tree (the paths from the root to each leaf node) */
  private def treeTerms(tree: Map[Char, CharSeq]): Set[String] = {
    val terms = mutable.Set[String]()

    def walk(node: CharSeq, prefix: String): Unit = {
      val term = prefix + node.ch
      if (node.isLeaf) terms += term
      node.other.values.foreach(walk(_, term))
    }
    tree.values.foreach(walk(_, ""))
    terms.toSet
  }

  "The term tree" should "have exactly the 'term_normalized' values of the index documents, except the stopwords" in {
    val stopwords = Set("amp", "www", "ano", "la", "dos", "or", "the")
    // the same terms read in the slow way: loading every stored document
    val expected: Set[String] = Using.resource(DirectoryReader.open(new MMapDirectory(Paths.get(decsPath)))) {
      reader =>
        val liveDocs = MultiBits.getLiveDocs(reader)
        val storedFields = reader.storedFields()
        (0 until reader.maxDoc()).iterator
          .filter(doc => (liveDocs == null) || liveDocs.get(doc))
          .map(doc => storedFields.document(doc).get("term_normalized"))
          .filterNot(stopwords.contains)
          .toSet
    }
    val terms = treeTerms(highlighter.terms)

    assert (terms.size > 500000)
    assert (terms == expected)
  }

  it should "not have the stopwords" in {
    val terms = treeTerms(highlighter.terms)
    assert (Set("the", "la", "amp", "www").forall(stopword => !terms.contains(stopword)))
  }

  "The cache" should "give the same results when a text is highlighted again" in {
    val str = "The health of children with asthma and diabetes"
    val first = highlighter.highlight("<em>", "</em>", str, config(Some("en")))
    val second = highlighter.highlight("<em>", "</em>", str, config(Some("en")))

    assert (first._2.nonEmpty)
    assert (second == first)
  }

  it should "keep the results of each configuration apart" in {
    val str = "The abattoirs are cruel!"

    assert (highlighter.highlight("<em>", "</em>", str, config(Some("en")))._2.nonEmpty)
    assert (highlighter.highlight("<em>", "</em>", str, config(Some("pt")))._2.isEmpty)
    assert (highlighter.highlight("<em>", "</em>", str, config(Some("en")))._2.nonEmpty)
  }

  it should "give the same results when used by several threads at the same time" in {
    val texts = Seq("The health of children with asthma", "A saúde da criança com asma e diabetes",
                    "Les filles de la république fédérative du Brésil", "Os matadouros/análise no Brasil",
                    "Vacinação e tuberculose em adolescentes", "The abattoirs and matadouros are cruel!")
    val jobs: Seq[(String, Config)] = for {
      text <- texts
      lang <- Seq(None, Some("en"), Some("pt"), Some("fr"))
    } yield (text, config(lang))
    // expected results are calculated by another highlighter, so they do not come from the tested cache
    val other = new Highlighter(decsPath)
    val expected = try jobs.map { case (text, conf) => other.highlight("<em>", "</em>", text, conf) }
                   finally other.close()

    val pool = Executors.newFixedThreadPool(8)
    try {
      val tasks = (1 to 20).flatMap(_ => jobs.zipWithIndex).map {
        case ((text, conf), index) =>
          pool.submit(new Callable[Boolean] {
            override def call(): Boolean = highlighter.highlight("<em>", "</em>", text, conf) == expected(index)
          })
      }
      assert (tasks.forall(_.get(60, TimeUnit.SECONDS)))
    } finally pool.shutdown()
  }
}
