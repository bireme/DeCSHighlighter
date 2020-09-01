package org.bireme.dh

import java.util.Date

case class CheckTime(funcName: String) {
  private val begin = new Date().getTime

  def mark(): Unit = {
    val now = new Date().getTime
    println(s"++ $funcName ${now - begin} ms")
  }
}
