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

  /**
    * Do initial web app configuration
    * @param config servlet config
    */
  override def init(config: ServletConfig): Unit = {
    super.init(config)

    val decsPath: String = config.getServletContext.getInitParameter("DECS_PATH")
    if ((decsPath == null) || decsPath.isEmpty )
      throw new NullPointerException("DECS_PATH")

    highlighter = new Highlighter(decsPath)
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
      val doc1 = doc.replaceAll("<span class=['\"]decs['\"] onmouseover=[^>]+?>([^<].+?)</span>", "$1")
      /*request.getParameterMap.asScala.foreach {
        case (k,v) => println(s"$k=${v.headOption.getOrElse("")}")
      }*/
      val scanLang: Option[String] = Option(request.getParameter("scanLang")).flatMap(x => if ("same".equals(x)) None else Some(x))
      val outLang: Option[String] = Option(request.getParameter("outLang")).flatMap(x => if ("same".equals(x)) None else Some(x))
      val pubType: Option[Char] = Option(request.getParameter("pubType")).flatMap(x => if ("all".equals(x)) None else x.headOption)
      val scanDescriptors: Boolean = true
      val scanSynonyms: Boolean = Option(request.getParameter("scanSynonyms")).exists("true".equals)
      val onlyPreCod: Boolean = Option(request.getParameter("onlyPreCod")).exists("true".equals)
      val conf = Config(scanLang, outLang, pubType, scanDescriptors, scanSynonyms, onlyPreCod)
      val (marked: String, _, set: Seq[String]) = highlighter.highlight(presu, doc1, conf)

      restoreState(html, conf).replace("{{text}}", marked).replace("{{descriptors}}", set.mkString("\n"))
    }
    response.setCharacterEncoding("UTF-8")
    response.setContentType("text/html;charset=UTF-8")

    val writer: PrintWriter = response.getWriter
    writer.write(result)
    writer.close()
  }

  private def restoreState(htm: String,
                           conf: Config): String = {
    val conv = Map("en" -> "english", "es" -> "spanish", "pt" -> "portuguese", "fr" -> "french")

    val h1 = conf.scanLang.map {
      lang =>
        val lang1 = conv.getOrElse(lang, "")
        htm.replace("option id=\"scanLang\" value=\"" + lang + "\">" + lang1 + "</option>",
          "option id=\"scanLang\" value=\"" + lang + "\" selected>" + lang1 + "</option>")
    }.getOrElse(htm)
    val h2 = conf.outLang.map {
      lang =>
        val lang1 = conv.getOrElse(lang, "")
        h1.replace("option id=\"outLang\" value=\"" + lang + "\">" + lang1 + "</option>",
          "option id=\"outLang\" value=\"" + lang + "\" selected>" + lang1 + "</option>")
    }.getOrElse(h1)
    val h3 = conf.pubType.map {
      ch =>
        h2.replace("<option id=\"pubType\" value=\"" + ch + "\">" + ch + "</option>",
          "<option id=\"pubType\" value=\"" + ch + "\" selected>" + ch + "</option>")
    }.getOrElse(h2)
    val h4 = if (conf.scanSynonyms) {
      h3.replace("<input type=\"checkbox\" id=\"scanSynonyms\" name=\"scanSynonyms\" value=\"scanSynonyms\">",
      "<input type=\"checkbox\" id=\"scanSynonyms\" name=\"scanSynonyms\" value=\"scanSynonyms\" checked>")
    } else h3
    val h5 = if (conf.onlyPreCod) {
      h4.replace("<input type=\"checkbox\" id=\"onlyPreCod\" name=\"onlyPreCod\" value=\"onlyPreCod\">",
        "<input type=\"checkbox\" id=\"onlyPreCod\" name=\"onlyPreCod\" value=\"onlyPreCod\" checked>")
    } else h4

    h5
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
      |
      |      var hiddenField = document.createElement("input");
      |      hiddenField.setAttribute("type", "hidden");
      |      hiddenField.setAttribute("name", "document");
      |      hiddenField.setAttribute("value", strip);
      |      form.appendChild(hiddenField);
      |
      |      var e1 = document.getElementById("scanLang");
      |      var option1 = e1.options[e1.selectedIndex];
      |      var data1 = option1.getAttribute("value");
      |      var hiddenField1 = document.createElement("input");
      |      hiddenField1.setAttribute("type", "hidden");
      |      hiddenField1.setAttribute("name", "scanLang");
      |      hiddenField1.setAttribute("value", data1);
      |      form.appendChild(hiddenField1);
      |
      |      var e2 = document.getElementById("outLang");
      |      var option2 = e2.options[e2.selectedIndex];
      |      var data2 = option2.getAttribute("value");
      |      var hiddenField2 = document.createElement("input");
      |      hiddenField2.setAttribute("type", "hidden");
      |      hiddenField2.setAttribute("name", "outLang");
      |      hiddenField2.setAttribute("value", data2);
      |      form.appendChild(hiddenField2);
      |
      |      var e3 = document.getElementById("pubType");
      |      var option3 = e3.options[e3.selectedIndex];
      |      var data3 = option3.getAttribute("value");
      |      var hiddenField3 = document.createElement("input");
      |      hiddenField3.setAttribute("type", "hidden");
      |      hiddenField3.setAttribute("name", "pubType");
      |      hiddenField3.setAttribute("value", data3);
      |      form.appendChild(hiddenField3);
      |
      |      var e4 = document.getElementById("scanSynonyms");
      |      var checked4 = e4.checked
      |      var hiddenField4 = document.createElement("input");
      |      hiddenField4.setAttribute("type", "hidden");
      |      hiddenField4.setAttribute("name", "scanSynonyms");
      |      hiddenField4.setAttribute("value", checked4.toString());
      |      form.appendChild(hiddenField4);
      |
      |      var e5 = document.getElementById("onlyPreCod");
      |      var checked5 = e5.checked
      |      var hiddenField5 = document.createElement("input");
      |      hiddenField5.setAttribute("type", "hidden");
      |      hiddenField5.setAttribute("name", "onlyPreCod");
      |      hiddenField5.setAttribute("value", checked5.toString());
      |      form.appendChild(hiddenField5);
      |
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
      |
      |&nbsp;
      |<label for="scanLang">Scan language:</label>
      |
      |<select id="scanLang">
      |  <option id="scanLang" value="same">all languages</option>
      |  <option id="scanLang" value="en">english</option>
      |  <option id="scanLang" value="es">spanish</option>
      |  <option id="scanLang" value="pt">portuguese</option>
      |  <option id="scanLang" value="fr">french</option>
      |</select>
      |&nbsp;&nbsp;&nbsp;&nbsp;
      |
      |<label for="outLang">Output language:</label>
      |
      |<select id="outLang">
      |  <option id="outLang" value="same">same of the text</option>
      |  <option id="outLang" value="en">english</option>
      |  <option id="outLang" value="es">spanish</option>
      |  <option id="outLang" value="pt">portuguese</option>
      |  <option id="outLang" value="fr">french</option>
      |</select>
      |&nbsp;&nbsp;&nbsp;&nbsp;
      |
      |<label for="pubType">Publication type:</label>
      |
      |<select id="pubType">
      |  <option id="pubType" value="all">all</option>
      |  <option id="pubType" value="h">h</option>
      |  <option id="pubType" value="q">q</option>
      |  <option id="pubType" value="t">t</option>
      |</select>
      |&nbsp;&nbsp;&nbsp;&nbsp;
      |
      |<input type="checkbox" id="scanSynonyms" name="scanSynonyms" value="scanSynonyms">
      |<label for="scanSynonyms">Scan synonyms</label>
      |&nbsp;&nbsp;&nbsp;&nbsp;
      |
      |<input type="checkbox" id="onlyPreCod" name="onlyPreCod" value="onlyPreCod">
      |<label for="onlyPreCod">Only precodified</label>
      |&nbsp;&nbsp;&nbsp;&nbsp;
      |
      |<nav>
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
