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
      val uid: String = getField(rec, 480).headOption.getOrElse(("", ""))._2
      val field105: String = getField(rec, 105).headOption.getOrElse(("", ""))._1.trim
      val field106: Seq[String] = getField(rec, 106).map(_._1)

      val documents: Seq[Document] =
        if ((field106.length == 2) && field106.contains("c") && field106.contains("h")) getCheckTags(id, uid, rec)
        else if ((field106.length == 2) && field106.contains("h") && field106.contains("g")) getGeographics(id, uid, rec)
        else field105 match {
          case "Q" => getQualifiers(id, uid, rec)
          case "T" =>
            if ((field106.length == 2) && field106.contains("h") && field106.contains("l")) getPubTypes(id, uid, rec)
            else Seq()
          case "H" =>
            if ((field106.length == 2) &&
              (field106.contains("c") && field106.contains("h")) ||
              (field106.contains("h") && field106.contains("g"))) Seq()
            else getMainHeadings(id, uid, rec)
          case _ => Seq()
        }
      documents.foreach(writer.addDocument)
    }
  }

  private def getCheckTags(id: String,
                           uid: String,
                           rec: Record): Seq[Document] = createDocs(id, uid, "checkTag", rec)

  private def getGeographics(id: String,
                             uid: String,
                             rec: Record): Seq[Document] = createDocs(id, uid, "geographic", rec)

  private def getQualifiers(id: String,
                            uid: String,
                            rec: Record): Seq[Document] = createDocs(id, uid, "qualifier", rec)

  private def getPubTypes(id: String,
                          uid: String,
                          rec: Record): Seq[Document] = createDocs(id, uid, "pubType", rec)

  private def getMainHeadings(id: String,
                              uid: String,
                              rec: Record): Seq[Document] = createDocs(id, uid, "mainHeading", rec)

  private def createDocs(id: String,
                         uid: String,
                         termType: String,
                         rec: Record): Seq[Document] = {
    val documents: Seq[Document] =
      getPtDocument(id, uid, rec) ++
      getEsDocument(id, uid, rec) ++
      getFrDocument(id, uid, rec) ++
      getEnDocument(id, uid, rec)

    documents.foreach(_.add(new StringField("termType", termType, Field.Store.YES)))
    documents
  }

  private def getEnDocument(id: String,
                            uid: String,
                            rec: Record): Seq[Document] = {
    getDocument(id, uid, 1, 'i', "en", rec)
  }

  private def getEsDocument(id: String,
                            uid: String,
                           rec: Record): Seq[Document] = {
    getDocument(id, uid, 2, 'e', "es", rec)
  }

  private def getPtDocument(id: String,
                            uid: String,
                            rec: Record): Seq[Document] = {
    getDocument(id, uid, 3, 'p', "pt", rec)
  }

  private def getFrDocument(id: String,
                            uid: String,
                            rec: Record): Seq[Document] = {
    getDocument(id, uid, 16, 'f', "fr", rec)
  }

  private def getDocument(id: String,
                          uid: String,
                          field: Int,
                          subFld: Char,
                          lang: String,
                          rec: Record): Seq[Document] = {
    val mainHeadings: Seq[(IndexableField, IndexableField)] = getMainHeadings(field, rec)
    val entryTerms: Seq[(IndexableField, IndexableField)] = getEntryTerms(subFld, rec)

    mainHeadings.map {
      fld =>
        val doc = new Document()
        doc.add(new StringField("id", id, Field.Store.YES))
        doc.add(new StringField("uniqueId", uid, Field.Store.YES))
        doc.add(new StringField("lang", lang, Field.Store.YES))
        doc.add(fld._1)
        doc.add(fld._2)
        doc
    } ++
      entryTerms.map {
        fld =>
          val doc = new Document()
          doc.add(new StringField("id", id, Field.Store.YES))
          doc.add(new StringField("uniqueId", uid, Field.Store.YES))
          doc.add(new StringField("lang", lang, Field.Store.YES))
          doc.add(new StringField("synonym", "true", Field.Store.YES))
          doc.add(fld._1)
          doc.add(fld._2)
          doc
      }
  }

  private def getMainHeadings(field: Int,
                              rec: Record): Seq[(IndexableField, IndexableField)] = {
    getField(rec, field).map {
      x => (
        new StringField("term", x._1, Field.Store.YES),
        new StringField("term_normalized", x._2, Field.Store.YES)
      )
    }
  }

  /**
    * Retrieve entry terms (synonyms) from an Isis record
    * @param subFld subfield tag which specify the language
    * @param rec Isis record
    * @return a sequence of entryTerms (synonyms) of form: (normalized synonym, original synonym)
    */
  private def getEntryTerms(subFld: Char,
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
        val content: String = fld.getContent.trim

        // Remove qualifier character
        val content1: String = if (content(0) == '/') content.substring(1) else content
        seq :+ (content1, uniformString(content1))
    }
  }
}
