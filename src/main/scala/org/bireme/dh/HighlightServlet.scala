/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import java.io.PrintWriter

import play.api.libs.json._
import javax.servlet.ServletConfig
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import scala.collection.immutable.ListMap
import scala.collection.mutable

/**
  * Servlet to highlight an input text.
  *
  * author: Heitor Barbieri
  * date: September - 2018
  */
class HighlightServlet extends HttpServlet {
  var highlighter: Highlighter = _

  /**
    * Do initial web app configuration
    * @param config servlet config object
    */
  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val decsPath: String = config.getServletContext.getInitParameter("DECS_PATH")
    if ((decsPath == null) || decsPath.isEmpty)
      throw new NullPointerException(s"DECS_PATH = $decsPath")

    highlighter = new Highlighter(decsPath)
    println("HighlightServlet is listening ...")
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
    if ((doc == null) || doc.isEmpty ) response.sendError(400, "Missing document parameter")
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

      // Highlight the input text
      val (marked: String, seq: Seq[(Int, Int, String, String, String)], set: Seq[String]) =
        highlighter.highlight(prefix, suffix, doc, conf)
      val result: mutable.Map[String, JsValue] = mutable.SeqMap[String, JsValue]()

      //println(s"marked=$marked set=$set")

      // Show all output (text, positions and descriptors) if the showText, showPositions and showDescriptors parameters
      // are absent.
      if (!showText && !showPositions && !showDescriptors) {
        result += "text" -> JsString(marked)
        result += "positions" -> JsArray(seq.map(
          elem => JsObject(ListMap("begin" -> JsNumber(elem._1), "end" -> JsNumber(elem._2), "id" -> JsString(elem._3),
                               "descriptor" -> JsString(elem._4), "original" -> JsString(elem._5)))))
        result += ("descriptors" -> JsArray(set.map(d => JsString(d))))
      } else {
        if (showText) result += "text" -> JsString(marked)
        if (showPositions) result += "positions" -> JsArray(seq.map(
          elem => JsObject(ListMap("begin" -> JsNumber(elem._1), "end" -> JsNumber(elem._2), "id" -> JsString(elem._3),
                               "descriptor" -> JsString(elem._4), "original" -> JsString(elem._5)))))
        if (showDescriptors) result += "descriptors" -> JsArray(set.map(d => JsString(d)))
      }
      response.setContentType("application/json")
      response.setCharacterEncoding("utf-8")

      // Transform the json object into a String and print it
      val resultStr = Json.stringify(JsObject(result))
      val writer: PrintWriter = response.getWriter
      writer.write(resultStr)
      writer.close()
    }
  }
}
