/*=========================================================================

    DeCSHighlighter © Pan American Health Organization, 2018.
    See License at: https://github.com/bireme/DecsHighlighter/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.dh

import java.io.PrintWriter

import javax.servlet.ServletConfig
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

/**
  * Example web application to show the hightlighted text given an input text.
  *
  * author: Heitor Barbieri
  * date: September - 2018
  **/
class HighlightWebServlet extends HttpServlet {

  var highlighter: Highlighter = _
  var tree: Map[Char, CharSeq] = _

  /**
    * Do initial web app configuration
    * @param config servlet config
    */
  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val decsPath: String = config.getServletContext.getInitParameter("DECS_PATH")
    if ((decsPath == null) || decsPath.isEmpty )
      throw new NullPointerException("DECS_PATH")

    val terms: Predef.Map[String, String] = Tools.decs2Set(decsPath)

    highlighter = new Highlighter()
    tree = highlighter.createTermTree(terms)
    println("HighlightWebServlet is listening ...")
  }

  private def presu(term: String): String = s"<span class='decs' onmouseover='showDescription($term)'>$term</span>"

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
    request.setCharacterEncoding("UTF-8")

    val doc: String = request.getParameter("document")
    val result: String = if ((doc == null) || doc.isEmpty ) {
      html.replace("{{text}}", "").replace("{{descriptors}}", "")
    } else {
      val (marked: String, _, set: Seq[String]) = highlighter.highlight(presu, doc, tree)
      html.replace("{{text}}", marked).replace("{{descriptors}}", set.mkString("\n"))
    }
    response.setCharacterEncoding("UTF-8")
    response.setContentType("text/html;charset=UTF-8")

    val writer: PrintWriter = response.getWriter
    writer.write(result)
    writer.close()
  }

  // Html that will be shown by the application
  val html: String =
    """
      |<!DOCTYPE html>
      |<html lang="en">
      |<head>
      |    <meta charset="UTF-8">
      |    <title>DeCS Descriptors</title>
      |    <style>
      |* {
      |    box-sizing: border-box;
      |}
      |
      |body {
      |    font-family: Arial, Helvetica, sans-serif;
      |}
      |
      |h3 {
      |   color: white;
      |}
      |
      |/* Style the header */
      |header {
      |    background-color: #639c63;
      |    padding: 1px;
      |    text-align: center;
      |    font-size: 30px;
      |    color: white;
      |}
      |
      |
      |/* Create two columns/boxes that floats next to each other */
      |nav {
      |    float: left;
      |    width: 70%;
      |    height: 700px; /* only for demonstration, should be removed */
      |    background: #c6dac9;
      |    padding: 20px;
      |}
      |
      |
      |article {
      |    float: left;
      |    width: 30%;
      |    height: 700px;  /* only for demonstration, should be removed */
      |    background-color: #c6dac9;
      |    padding: 20px;
      |    overflow-x: hidden;
      |    overflow-y: auto;
      |}
      |
      |/* Clear floats after the columns */
      |section:after {
      |    content: "";
      |    display: table;
      |    clear: both;
      |}
      |
      |/* Style the footer */
      |footer {
      |    background-color: #639c63;
      |    padding: 10px;
      |    text-align: right;
      |    color: white;
      |}
      |
      |.decs {
      |    color: blue;
      |}
      |
      |</style>
      |
      |<script>
      |    function clearDivs() {
      |      document.getElementById('div_text').innerHTML = "";
      |      document.getElementById('div_descriptors').innerHTML = "";
      |    }
      |
      |    function removePrefixSuffix(text) {
      |      var content1 = text.replace(/<span class=\"decs\">/g, "");
      |      var content2 = content1.replace(/<\/span>/g, "");
      |
      |      return content2;
      |    }
      |
      |    function stripX(html) {
      |      var doc = new DOMParser().parseFromString(html, 'text/html');
      |      var ret = doc.body.textContent || "";
      |      return ret;
      |    }
      |
      |    function sendText() {
      |      var content = document.getElementById('div_text').innerHTML;
      |      var strip = content; //strip(content);
      |      var form = document.createElement("form");
      |      form.setAttribute("method", "post");
      |      form.setAttribute("action", "#");
      |      var hiddenField = document.createElement("input");
      |      hiddenField.setAttribute("type", "hidden");
      |      hiddenField.setAttribute("name", "document");
      |      hiddenField.setAttribute("value", strip);
      |      form.appendChild(hiddenField);
      |      document.body.appendChild(form);
      |      form.submit();
      |    }
      |
      |</script>
      |
      |</head>
      |
      |<body>
      |<header>
      |<h2>DeCS Descriptors</h2>
      |</header>
      |
      |<section>
      |    <nav>
      |<h3>Your text</h3>
      |        <div id="div_text" name="docum" style="background-color:#f1f1f1; height:90%; overflow-y:auto;" contenteditable="true">{{text}}</div>
      |        <!--textarea id="div_text" name="docum" style="background-color:#f1f1f1;height: 90%;width: 100%;contenteditable= 'true'; resize: none;">{{text}}</textarea-->
      |    </nav>
      |
      |    <article>
      |        <h3> Descriptors/synonyms found</h3>
      |        <!--div id="div_descriptors" style="background-color:#f1f1f1;height: 90%;contenteditable=false; overflow-y: auto;">{{descriptors}}</div-->
      |        <textarea readonly="readonly" id="div_descriptors" style="background-color:#f1f1f1;height: 90%;width:100%; overflow-y: auto; resize:none;font-size: 15px;">{{descriptors}}</textarea>
      |    </article>
      |</section>
      |<footer>
      |    <button onclick="clearDivs()" >clear</button>
      |    <button onclick="sendText()">OK</button>
      |</footer>
      |</body>
      |</html>
    """.stripMargin
}