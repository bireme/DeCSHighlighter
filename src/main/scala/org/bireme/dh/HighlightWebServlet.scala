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
  var prefix: String = _
  var suffix: String = _

  /**
    * Do initial web app configuration
    * @param config serlvet config
    */
  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val decsPath: String = config.getServletContext.getInitParameter("DECS_PATH")
    if ((decsPath == null) || decsPath.isEmpty )
      throw new NullPointerException("DECS_PATH")

    val terms: Predef.Map[String, String] = Tools.decs2Set(decsPath)

    highlighter = new Highlighter()
    tree = highlighter.createTermTree(terms)
    prefix = config.getServletContext.getInitParameter("PREFIX")
    suffix = config.getServletContext.getInitParameter("SUFFIX")
    println("HighlightWebServlet is listening ...")
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
    val doc: String = request.getParameter("document")

    val result: String = if ((doc == null) || doc.isEmpty ) {
      html.replace("{{text}}", "").replace("{{descriptors}}", "")
    } else {
      val (marked: String, _, set: Seq[String]) = highlighter.highlight(prefix, suffix, doc, tree)
      //println(s"original=${doc.get}")
      //println(s"positions=$pos")
      //println(s"marked=$marked")
      html.replace("{{text}}", marked).replace("{{descriptors}}", set.mkString("\n"))
    }
    //println(s"\n\n\nresult=$result")
    response.setContentType("text/html")

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
      |/* Style the header */
      |header {
      |    background-color: #666;
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
      |    background: #ccc;
      |    padding: 20px;
      |}
      |
      |
      |article {
      |    float: left;
      |    width: 30%;
      |    height: 700px;  /* only for demonstration, should be removed */
      |    background-color: #ccc;
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
      |    background-color: #777;
      |    padding: 10px;
      |    text-align: right;
      |    color: white;
      |}
      |
      |</style>
      |
      |<script>
      |    function clearDivs()
      |    {
      |    document.getElementById('div_text').innerHTML = "";
      |    document.getElementById('div_descriptors').innerHTML = "";
      |    }
      |
      |    function sendText()
      |    {
      |    var content = document.getElementById('div_text').value;
      |    var form = document.createElement("form");
      |    form.setAttribute("method", "post");
      |    form.setAttribute("action", "/app");
      |    var hiddenField = document.createElement("input");
      |    hiddenField.setAttribute("type", "hidden");
      |    hiddenField.setAttribute("name", "document");
      |    hiddenField.setAttribute("value", content);
      |    form.appendChild(hiddenField);
      |    document.body.appendChild(form);
      |    form.submit();
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
      |        <!--div id="div_text" name="docum" style="background-color:#f1f1f1;height: 90%;" contenteditable="true">{{text}}</div-->
      |        <textarea id="div_text" name="docum" style="background-color:#f1f1f1;height: 90%;width: 100%;contenteditable= 'true'; resize: none;">{{text}}</textarea>
      |    </nav>
      |
      |    <article>
      |        <h3> Descriptors/synonyms found</h3>
      |        <!--div id="div_descriptors" style="background-color:#f1f1f1;height: 90%;contenteditable=false; overflow-y: auto;">{{descriptors}}</div-->
      |        <textarea readonly="readonly" id="div_descriptors" style="background-color:#f1f1f1;height: 90%;width:100%; overflow-y: auto; resize:none;">{{descriptors}}</textarea>
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