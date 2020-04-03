package org.bireme.dh

import java.io.File
import java.nio.file.Path

import bruma.master.{Master, MasterFactory, Record}
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Document, Field, StringField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig, IndexableField}
import org.apache.lucene.store.{Directory, FSDirectory}
import org.bireme.dh.Tools.uniformString

import scala.jdk.CollectionConverters._

object DeCS2Lucene extends App {
  private def usage(): Unit = {
    System.err.println("usage: DeCS2Lucene <options> ")
    System.err.println("options:")
    System.err.println("-isis=<isisPath> : path to DeCS Isis master")
    System.err.println("-lucene=<lucenePath> : path to Lucene DeCS index to be created")
  }
  if (args.length < 2) usage()

  val parameters = args.foldLeft[Map[String,String]](Map()) {
    case (map, par) =>
      val split = par.split(" *= *", 2)
      split.length match {
        case 1 => map + ((split(0).substring(2), ""))
        case _ => map + ((split(0).substring(1), split(1)))
      }
  }

  create(parameters("isis"), parameters("lucene"))

  val stopwords = Set("la", "foram", "amp", "www") // are common words and have other meanings in other languages


  def create(isisPath: String,
             lucenePath: String): Unit = {
    val mst: Master = MasterFactory.getInstance(isisPath).open()

    val analyzer: KeywordAnalyzer = new KeywordAnalyzer()
    val indexPath: Path = new File(lucenePath).toPath
    val directory: Directory = FSDirectory.open(indexPath)
    val config: IndexWriterConfig = new IndexWriterConfig(analyzer).setOpenMode(IndexWriterConfig.OpenMode.CREATE)
    val iwriter: IndexWriter = new IndexWriter(directory, config)

    mst.iterator().asScala.foreach(createDocuments(_, iwriter))
    mst.close()
    iwriter.forceMerge(1)
    iwriter.close()
    directory.close()
  }

  def createDocuments(rec: Record,
                      writer: IndexWriter): Unit = {
    if (rec.getStatus == Record.Status.ACTIVE) {
      val id: String = getField(rec, 99).head._2
      val uid: String = getField(rec, 480).headOption.getOrElse(("",""))._2
      val publType: String = getField(rec, 105) match {
        case set if set.isEmpty => ""
        case set => set.head._2
      }
      val preCod: Boolean = isPreCod(rec)
      val ptDoc: Seq[Document] = getPtDocument(id, uid, publType, preCod, rec)
      val esDoc: Seq[Document] = getEsDocument(id, uid, publType, preCod, rec)
      val enDoc: Seq[Document] = getEnDocument(id, uid, publType, preCod, rec)
      val frDoc: Seq[Document] = getFrDocument(id, uid, publType, preCod, rec)

      ptDoc.foreach(writer.addDocument)
      esDoc.foreach(writer.addDocument)
      enDoc.foreach(writer.addDocument)
      frDoc.foreach(writer.addDocument)
    }
  }

  private def isPreCod(rec: Record): Boolean = getField(rec, fldTag=106).exists(x => x._2.trim.head.equals('c'))

  private def getEnDocument(id: String,
                            uid: String,
                            publType: String,
                            preCod: Boolean,
                            rec: Record): Seq[Document] = {
    getDocument(id, uid, 1, 'i', "en", publType, preCod, rec)
  }

  private def getEsDocument(id: String,
                            uid: String,
                            publType: String,
                            preCod: Boolean,
                            rec: Record): Seq[Document] = {
    getDocument(id, uid, 2, 'e', "es", publType, preCod, rec)
  }

  private def getPtDocument(id: String,
                            uid: String,
                            publType: String,
                            preCod: Boolean,
                            rec: Record): Seq[Document] = {
    getDocument(id, uid, 3, 'p', "pt", publType, preCod, rec)
  }

  private def getFrDocument(id: String,
                            uid: String,
                            publType: String,
                            preCod: Boolean,
                            rec: Record): Seq[Document] = {
    getDocument(id, uid, 16, 'f', "fr", publType, preCod, rec)
  }

  private def getDocument(id: String,
                          uid: String,
                          field: Int,
                          subFld: Char,
                          lang: String,
                          publType: String,
                          preCod: Boolean,
                          rec: Record): Seq[Document] = {
    val descr: Seq[(IndexableField, IndexableField)] = getDescriptors(field, rec)
    val synonyms: Seq[(IndexableField, IndexableField)] = getSynonyms(subFld, rec)

    descr.map {
      fld =>
        val doc = new Document()
        doc.add(new StringField("id", id, Field.Store.YES))
        doc.add(new StringField("uniqueId", uid, Field.Store.YES))
        doc.add(new StringField("publicationType", publType, Field.Store.YES))
        doc.add(new StringField("preCod", if (preCod) "t" else "f", Field.Store.YES))
        doc.add(new StringField("lang", lang, Field.Store.YES))
        doc.add(new StringField("termType", "descriptor", Field.Store.YES))
        doc.add(fld._1)
        doc.add(fld._2)
        doc
    } ++
      synonyms.map {
        fld =>
          val doc = new Document()
          doc.add(new StringField("id", id, Field.Store.YES))
          doc.add(new StringField("uniqueId", uid, Field.Store.YES))
          doc.add(new StringField("publicationType", publType, Field.Store.YES))
          doc.add(new StringField("preCod", if (preCod) "t" else "f", Field.Store.YES))
          doc.add(new StringField("lang", lang, Field.Store.YES))
          doc.add(new StringField("termType", "synonym", Field.Store.YES))
          doc.add(fld._1)
          doc.add(fld._2)
          doc
      }
  }

  private def getDescriptors(field: Int,
                             rec: Record): Seq[(IndexableField, IndexableField)] = {
    getField(rec, field).map {
      x => (
        new StringField("term", x._1, Field.Store.YES),
        new StringField("term_normalized", x._2, Field.Store.YES)
      )
    }
  }

  private def getSynonyms(subFld: Char,
                          rec: Record): Seq[(IndexableField, IndexableField)] = {
    rec.getFieldList(50).asScala.flatMap(_.getTagSubfields(subFld).asScala.map {
      fld =>
        val content: String = fld.getContent
        (
          new StringField("term", content, Field.Store.YES),
          new StringField("term_normalized", uniformString(content), Field.Store.YES)
        )
    }).toSeq
  }

  /**
    * Retrieve a record field content from an Isis database
    * @param rec the Isis database record object
    * @param fldTag the tag of the field to be retrieved
    * @return a sequence of (field content,field content normalized)
    */
  private def getField(rec: Record,
                       fldTag: Int): Seq[(String, String)] = {
    rec.getFieldList(fldTag).asScala.foldLeft(Seq[(String, String)]()) {
      case (seq, fld) =>
        val content: String = fld.getContent
        seq :+ (content, uniformString(content))
    }
  }
}
