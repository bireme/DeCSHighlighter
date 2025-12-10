/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import jakarta.servlet.ServletConfig
import jakarta.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import org.slf4j.{Logger, LoggerFactory}

import java.io.PrintWriter
import play.api.libs.json._
import sttp.client4.httpclient.HttpClientSyncBackend
import sttp.model.Uri
import sttp.shared.Identity

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * Servlet to highlight an input text.
  *
  * author: Heitor Barbieri
  * date: September - 2018
  *
  */
class HighlightServlet extends HttpServlet {
  private var highlighter: Highlighter = _
  private var logger: Option[Logger] = None

  /**
    * Do initial web app configuration
    * @param config servlet config object
    */
  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val decsPath: String = config.getServletContext.getInitParameter("DECS_PATH")
    if ((decsPath == null) || decsPath.isEmpty)
      throw new NullPointerException(s"DECS_PATH = $decsPath")

    logger = Option(config.getServletContext.getInitParameter("SHOULD_LOG")) match {
      case Some(shouldLog) =>
        if (shouldLog.trim.toBoolean) Some(LoggerFactory.getLogger(classOf[HighlightServlet]))
        else None
      case None => None
    }

    highlighter = new Highlighter(decsPath)
    println("HighlightServlet is listening ...")
    if (logger.isDefined) println("Logging is activated.")
  }

  /**
    * Process get http requisition
    * @param request http request object
    * @param response http response object
    */
  override def doGet(request: HttpServletRequest,
                     response: HttpServletResponse): Unit = processRequest(request, response)

  /**
    * Process post http requisition
    * @param request http request object
    * @param response http response object
    */
  override def doPost(request: HttpServletRequest,
                      response: HttpServletResponse): Unit = processRequest(request, response)

  /**
    *  Process get or post requisition
    * @param request http request object
    * @param response http response object
    */
  private def processRequest(request: HttpServletRequest,
                             response: HttpServletResponse): Unit = {
    // Parse document parameter
    val doc = request.getParameter("document")
    if ((doc == null) || doc.isEmpty ) response.sendError(400, s"Missing document parameter. Parameters are: ${request.getParameterMap}")
    else {
      // Parse parameters
      val prefix0: String = request.getParameter("prefix")
      val suffix0: String = request.getParameter("suffix")
      val scanLang: Option[String] = Option(request.getParameter("scanLang")).flatMap {
        opt =>  opt match {
          case "en" | "es" | "pt" | "fr" => Some(opt)
          case _ => None
        }
      }
      val outLang: Option[String] = Option(request.getParameter("outLang")).flatMap {
        opt =>  opt match {
          case "en" | "es" | "pt" | "fr" => Some(opt)
          case _ => None
        }
      }
      val pubType: Char = Option(request.getParameter("pubType")).map(_.trim.toLowerCase.charAt(0)).getOrElse(0.toChar)
      val scanMainHeadings: Boolean = (pubType == 'h') ||
        Option(request.getParameter("scanMainHeadings")).map {
          opt => opt.isEmpty || (opt.toLowerCase.head == 't')
        }.getOrElse(pubType == 0.toChar)
      val scanEntryTerms: Boolean = (pubType != 0.toChar) ||
        Option(request.getParameter("scanEntryTerms")).map {
          opt => opt.isEmpty || (opt.toLowerCase.head == 't')
        }.getOrElse(pubType == 0.toChar)
      val scanQualifiers: Boolean =  (pubType == 'q') ||
        Option(request.getParameter("scanQualifiers")).map {
          opt => opt.isEmpty || (opt.toLowerCase.head == 't')
        }.getOrElse(pubType == 0.toChar)
      val scanPublicationTypes: Boolean = (pubType == 't') ||
        Option(request.getParameter("scanPublicationTypes")).map {
          opt => opt.isEmpty || (opt.toLowerCase.head == 't')
        }.getOrElse(pubType == 0.toChar)
      val scanCheckTags: Boolean = (pubType == 'c') ||
        Option(request.getParameter("scanCheckTags")).map {
          opt => opt.isEmpty || (opt.toLowerCase.head == 't')
        }.getOrElse(pubType == 0.toChar)
      val scanGeographics: Boolean = (pubType == 'g') ||
        Option(request.getParameter("scanGeographics")).map {
          opt => opt.isEmpty || (opt.toLowerCase.head == 't')
        }.getOrElse(pubType == 0.toChar)

      val conf: Config = {
        Config(scanLang, outLang, scanMainHeadings, scanEntryTerms, scanQualifiers, scanPublicationTypes,
          scanCheckTags, scanGeographics)
      }
//println(s"config=$conf")
      val prefix = if ((prefix0 == null) || prefix0.isEmpty ) "<em>" else prefix0
      val suffix = if ((suffix0 == null) || suffix0.isEmpty ) "</em>" else suffix0
      val showText: Boolean = Option(request.getParameter("showText"))
        .forall(x => x.isEmpty || (x.toLowerCase.head == 't'))
      val showPositions: Boolean = Option(request.getParameter("showPositions"))
        .forall(x => x.isEmpty || (x.toLowerCase.head == 't'))
      val showDescriptors: Boolean = Option(request.getParameter("showDescriptors"))
        .forall(x => x.isEmpty || (x.toLowerCase.head == 't'))
      val showScores: Boolean = Option(request.getParameter("showScores"))
        .forall(x => x.isEmpty || (x.toLowerCase.head == 't'))

      // Highlight the input text
      Try (highlighter.highlight(prefix, suffix, doc, conf)) match {
        case Success((marked: String, seq: Seq[(Int, Int, String, String, String, String)], terms: Seq[(String,Int,Double)])) =>
          val result: mutable.Map[String, JsValue] = mutable.SeqMap[String, JsValue]()

          // write the result in the log
          logResult(logger, request, doc, terms)

          // Show all output (text, positions and descriptors) if the showText, showPositions, showDescriptors and showScores parameters
          // are absent.
          if (!showText && !showPositions && !showDescriptors && !showScores) {
            result += "text" -> JsString(marked)
            result += "positions" -> JsArray(seq.map(
              elem => JsObject(ListMap("begin" -> JsNumber(elem._1), "end" -> JsNumber(elem._2), "id" -> JsString(elem._3),
                "decsId" -> JsString(elem._4),"descriptor" -> JsString(elem._6), "original" -> JsString(elem._6)))))
            result += ("descriptors" -> JsArray(terms.map(d => JsString(d._1))))
            result += "scores" -> JsArray(terms.map(
              elem => JsObject(ListMap("descriptor" -> JsString(elem._1), "quantity" -> JsNumber(elem._2), "score" -> JsNumber(elem._3)))))
          } else {
            if (showText) result += "text" -> JsString(marked)
            if (showPositions) result += "positions" -> JsArray(seq.map(
              elem => JsObject(ListMap("begin" -> JsNumber(elem._1), "end" -> JsNumber(elem._2), "id" -> JsString(elem._3),
                "decsId" -> JsString(elem._4), "descriptor" -> JsString(elem._5), "original" -> JsString(elem._6)))))
            if (showDescriptors) result += "descriptors" -> JsArray(terms.map(d => JsString(d._1)))
            if (showScores) result += "scores" -> JsArray(terms.map(
              elem => JsObject(ListMap("descriptor" -> JsString(elem._1), "quantity" -> JsNumber(elem._2), "score" -> JsNumber(elem._3)))))
          }
          response.setContentType("application/json")
          response.setCharacterEncoding("utf-8")

          // Transform the json object into a String and print it
          val resultStr = Json.stringify(JsObject(result))
          val writer: PrintWriter = response.getWriter
          writer.write(resultStr)
          writer.close()
        case Failure(exception: Throwable) =>
          val errMesg: String = exception match {
            case _: java.lang.StackOverflowError => "Sorry, your text is too long!"
            case _ => exception.getMessage
          }
          response.sendError(500, errMesg)
      }
    }
  }

  private def logResult(logger: Option[Logger],
                        request: HttpServletRequest,
                        document: String,
                        descriptors: Seq[(String, Int, Double)]): Unit = {
    logger.foreach {
      logr =>
        // https://stackoverflow.com/questions/29910074/how-to-get-client-ip-address-in-java-httpservletrequest
        val ipAddress: String = Option(request.getHeader("X-FORWARDED-FOR")) match {
          case Some(address) =>
            if (address.contains(",")) address.split(",", 2)(0)
            else address
          case None => Option(request.getRemoteAddr).getOrElse("unknown")
        }
        val docLen: Int = document.length
        val descr: String = descriptors.map(_._1).mkString(";")

        logr.info(s" IP:$ipAddress  DocumentLen:$docLen  Found:${descriptors.size}  Descriptors:$descr")
    }
  }
}

object HighlightServletTest {
  def main(args: Array[String]): Unit = {
    import sttp.client4._

    val uri: Uri = uri"http://172.17.1.139:9090/decshighlighter/serv"
    val text: String =
      """Diabesity
        |Metastasis is the growth of cancer cells in sites distant from the organ from which
        |they originated, and its occurrence indicates a poor prognosis. In the oral cavity
        |they are rare. They can occur in the soft tissues, jaws or both, and are of
        |significant clinical importance, as they indicate a disseminated stage of cancer,
        |and may be the only symptom of an underlying malignancy that has not yet been
        |diagnosed. Therefore, it is essential that the dental surgeon is familiar with the
        |clinical aspect and radiographic findings most associated with this pathology. The
        |aim of this narrative literature review is to compile the main aspects about oral
        |metastases for the clinician's work. The pathogenesis of metastases to the oral
        |cavity is complex and not fully understood. The most frequent primary sites are
        |breast cancer, for metastases in the jaw bones; and lung cancer, for deposits in
        |oral soft tissues. The most affected oral regions are the posterior area of the jaw
        |and the gingival tissue. Metastases in bone are more prevalent than in soft
        |tissues. In general, men are more affected, especially in the fifth to seventh
        |decade of life, and the clinical appearance of metastases is variable, resembling
        |inflammatory or hyperplastic lesions, but with rapid growth. Radiographic findings
        |are nonspecific and may present characteristics such as “moth-eaten” bone and
        |irregular increase in periodontal membrane spaces. This review concludes that it
        |is essential for the dentist to know and include metastatic lesions in the differential
        |diagnosis of oral pathologies, given their high degree of clinical relevance.
        |Keywords: Neoplasm Metastasis; Mouth Neoplasms; Pathology, Oral.""".stripMargin

    val text1: String =
      """Boletim epidemiológico que apresenta informações sobre os casos de HIV no estado de Goiás
        |e tem como objetivo descrever o perfil epidemiológico, tendências da infecção na população
        |adulta entre os anos 2020 a 2024 e fornecer subsídios para a tomada de decisão, medidas de
        |vigilância, prevenção e controle da infecção pelo HIV em sua quinta década de epidemia.
        |Trata-se de um estudo descritivo, a partir dos dados obtidos do Sistema de Informação de
        |Agravos de Notificação (SINAN) e Sistema de Informação de Mortalidade (SIM). Foram tabulados
        |os dados diagnosticados e notificados por HIV/Aids por município de residência""".stripMargin

    val request: Request[Either[String, String]] = basicRequest
      .body(Map("showText" -> "f",
        "showPositions" -> "f",
        "showDescriptors" -> "f",
        "showScores" -> "t",
        "document" -> text1))
      .post(uri)

    val backend: WebSocketSyncBackend = HttpClientSyncBackend()
    val response: Identity[Response[Either[String, String]]] = request.send(backend)

    println(response.body)
    println(response.headers)
  }
}