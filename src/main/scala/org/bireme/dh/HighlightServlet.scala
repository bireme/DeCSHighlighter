/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import java.io.PrintWriter

import play.api.libs.json._

import javax.servlet.ServletConfig
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

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
      val pubType: Option[Char] = Option(request.getParameter("pubType")).map(_.trim.toLowerCase.charAt(0)).flatMap {
        opt =>  opt match {
          case 'h' | 'q' | 't' => Some(opt)
          case _ => None
        }
      }
      val scanDescriptors: Boolean = true
      val scanSynonyms: Boolean = Option(request.getParameter("scanSynonyms"))
        .map(x => x.isEmpty || (x.toLowerCase.head == 't')).getOrElse(true)
      val onlyPreCod: Boolean = Option(request.getParameter("onlyPreCod"))
        .map(x => x.isEmpty || (x.toLowerCase.head == 't')).getOrElse(false)
      val conf: Config = Config(scanLang, outLang, pubType, scanDescriptors, scanSynonyms, onlyPreCod)

      val prefix = if ((prefix0 == null) || prefix0.isEmpty ) "<em>" else prefix0
      val suffix = if ((suffix0 == null) || suffix0.isEmpty ) "</em>" else suffix0
      val showText: Boolean = Option(request.getParameter("showText"))
        .map(x => x.isEmpty || (x.toLowerCase.head == 't')).getOrElse(true)
      val showPositions: Boolean = Option(request.getParameter("showPositions"))
        .map(x => x.isEmpty || (x.toLowerCase.head == 't')).getOrElse(true)
      val showDescriptors: Boolean = Option(request.getParameter("showDescriptors"))
        .map(x => x.isEmpty || (x.toLowerCase.head == 't')).getOrElse(true)

      println(s"scanSynonyms=$scanSynonyms showText=$showText showPositions=$showPositions showDescriptors=$showDescriptors")

      // Highlight the input text
      val (marked: String, seq: Seq[(Int, Int, String, String)], set: Seq[String]) =
        highlighter.highlight(prefix, suffix, doc, conf)
      val result: mutable.Map[String, JsValue] = mutable.Map[String, JsValue]()

      // Show all output (text, positions and descriptors) if the showText, showPositions and showDescriptors parameters
      // are absent.
      if (!showText && !showPositions && !showDescriptors) {
        result += "text" -> JsString(marked)
        result += "positions" -> JsArray(seq.map(
          elem => JsObject(Map("begin" -> JsNumber(elem._1), "end" -> JsNumber(elem._2), "id" -> JsString(elem._3),
                               "descriptor" -> JsString(elem._4)))))
        result += ("descriptors" -> JsArray(set.map(d => JsString(d))))
      } else {
        if (showText) result += "text" -> JsString(marked)
        if (showPositions) result += "positions" -> JsArray(seq.map(
          elem => JsObject(Map("begin" -> JsNumber(elem._1), "end" -> JsNumber(elem._2), "id" -> JsString(elem._3)))))
        if (showDescriptors) result += "descriptors" -> JsArray(set.map(d => JsString(d)))
      }
      response.setContentType("application/json")

      // Transform the json object into a String and print it
      val resultStr = Json.stringify(JsObject(result))
      val writer: PrintWriter = response.getWriter
      writer.write(resultStr)
      writer.close()
    }
  }
}
