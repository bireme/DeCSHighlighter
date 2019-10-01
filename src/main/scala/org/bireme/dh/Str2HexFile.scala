package org.bireme.dh

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Paths, StandardOpenOption}

import scala.io.Source

/**
*  Class that read a file having characters (0 to 9 and a,b,c,d,e,f) and writes each par of characters into
  *  a hexadecimal number and then write into the destination file.
  *  It:
  *    - skips white spaces
  *    - takes 2 letters (hexadecimal number representation) and converts them into the hexadecimal number
  *    - can consider little endian or bin endian byte order
  */
object Str2HexFile extends App {
  private def usage(): Unit = {
    System.err.println("usage: Str2HexFile -in=<inFile> -out=<outFile> [--swapBytes]")
    System.exit(1)
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

  convert(parameters("in"), parameters("out"), parameters.contains("swapBytes"))

  private def convert(in: String,
                      out: String,
                      swapBytes: Boolean): Unit = {
    val inSrc = Source.fromFile(in, "utf-8")

    val inStr: String = inSrc.getLines().mkString("").replace(" ", "")
    inSrc.close()
    assert(inStr.matches("[0-9a-f]+"))

    val strLen = inStr.length
    assert(strLen % 2 == 0)

    val bb: ByteBuffer = createBuffer(inStr, 0, strLen, ByteBuffer.allocate(strLen), swapBytes)

    val fc: FileChannel = FileChannel.open(Paths.get(out),
                                           StandardOpenOption.CREATE,
                                           StandardOpenOption.TRUNCATE_EXISTING,
                                           StandardOpenOption.WRITE)
    fc.write(bb.flip())
    fc.close()
  }

  @scala.annotation.tailrec
  private def createBuffer(in: String,
                           pos: Int,
                           len: Int,
                           bb: ByteBuffer,
                           swapBytes: Boolean): ByteBuffer = {
    if (pos < len - 2) {
      val prefix: String = in.substring(pos, pos + 2)
      val value = Integer.parseInt(prefix, 16).toByte
      val prefix2: String = in.substring(pos + 2, pos + 4)
      val value2 = Integer.parseInt(prefix2, 16).toByte

      if (swapBytes) {
        bb.put(value2)
        bb.put(value)
      } else {
        bb.put(value)
        bb.put(value2)
      }
      createBuffer(in, pos + 4, len, bb, swapBytes)
    } else if (pos == len - 2) {
      val prefix: String = in.substring(pos, pos + 2)
      val value = Integer.parseInt(prefix, 16).toByte
      bb.put(value)
      createBuffer(in, pos + 2, len, bb, swapBytes)
    } else bb
  }
}
