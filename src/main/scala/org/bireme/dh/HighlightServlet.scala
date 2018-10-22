/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import java.io.PrintWriter

import io.circe.Json
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
  val highlighter: Highlighter = new Highlighter()
  var tree: Map[Char, CharSeq] = _

  /**
    * Do initial web app configuration
    * @param config servlet config object
    */
  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val decsPath: String = config.getServletContext.getInitParameter("DECS_PATH")
    if ((decsPath == null) || decsPath.isEmpty )
      throw new NullPointerException(s"DECS_PATH = $decsPath")

    val terms: Predef.Map[String, String] = Tools.decs2Set(decsPath)

    tree = highlighter.createTermTree(terms)

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
      val prefix0 = request.getParameter("prefix")
      val suffix0 = request.getParameter("suffix")
      val prefix = if ((prefix0 == null) || prefix0.isEmpty ) "<em>" else prefix0
      val suffix = if ((suffix0 == null) || suffix0.isEmpty ) "</em>" else suffix0
      val sText: String = request.getParameter("showText")
      val sPositions: String = request.getParameter("showPositions")
      val sDescriptors: String = request.getParameter("showDescriptors")
      val showText: Boolean = (sText != null) && (sText.isEmpty || sText.toBoolean)
      val showPositions: Boolean = (sPositions != null) && (sPositions.isEmpty || sPositions.toBoolean)
      val showDescriptors: Boolean = (sDescriptors != null) && (sDescriptors.isEmpty || sDescriptors.toBoolean)

      // Highlight the input text
      val (marked: String, seq: Seq[(Int, Int, String, String)], set: Seq[String]) = highlighter.highlight(prefix, suffix, doc, tree)
      val result: mutable.Buffer[(String, Json)] = mutable.Buffer[(String, Json)]()

      // Show all output (text, positions and descriptors) if the showText, showPositions and showDescriptors parameters
      // are absent.
      if (!showText && !showPositions && !showDescriptors) {
        result += "text" -> Json.fromString(marked)
        result += ("positions" -> Json.fromValues(seq.map(
          elem => Json.obj("begin" -> Json.fromInt(elem._1), "end" -> Json.fromInt(elem._2),
          "id" -> Json.fromString(elem._3), "descriptor" -> Json.fromString(elem._4)))))
        result += ("descriptors" -> Json.fromValues(set.map(d => Json.fromString(d))))
      } else {
        if (showText) result += "text" -> Json.fromString(marked)
        if (showPositions) result += ("positions" -> Json.fromValues(seq.map(
          elem => Json.obj("begin" -> Json.fromInt(elem._1), "end" -> Json.fromInt(elem._2), "id" -> Json.fromString(elem._3)))))
        if (showDescriptors) result += ("descriptors" -> Json.fromValues(set.map(d => Json.fromString(d))))
      }
      response.setContentType("application/json")

      // Transform the json object into a String and print it
      val resultStr = Json.obj(result: _*).spaces2
      val writer: PrintWriter = response.getWriter
      writer.write(resultStr)
      writer.close()
    }
  }
}