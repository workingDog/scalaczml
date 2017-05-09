package com.kodekutters.czml

import java.io.{IOException, File, PrintWriter}

import com.kodekutters.czml.czmlProperties._

import play.api.libs.json.Json


/**
  * basic supporting utilities
  */
object Util {

  // constants
  val COMPLETE = "COMPLETE"
  val BELOW_ELLIPSOID_HORIZON = "BELOW_ELLIPSOID_HORIZON"
  val ABOVE_ELLIPSOID_HORIZON = "ABOVE_ELLIPSOID_HORIZON"
  val LEFT = "LEFT"
  val CENTER = "CENTER"
  val RIGHT = "RIGHT"
  val BOTTOM = "BOTTOM"
  val TOP = "TOP"
  val FILL = "FILL"
  val OUTLINE = "OUTLINE"
  val FILL_AND_OUTLINE = "FILL_AND_OUTLINE"
  val UNBOUNDED = "UNBOUNDED"
  val CLAMPED = "CLAMPED"
  val LOOP_STOP = "LOOP_STOP"
  val SYSTEM_CLOCK = "SYSTEM_CLOCK"
  val SYSTEM_CLOCK_MULTIPLIER = "SYSTEM_CLOCK_MULTIPLIER"
  val TICK_DEPENDENT = "TICK_DEPENDENT"

  // interpolators
  val HERMITE = "HERMITE"
  val LAGRANGE = "LAGRANGE"
  val LINEAR = "LINEAR"


  def isEmpty(x: String) = x != null && x.trim.nonEmpty

  def isAllDigits(x: String) = x forall Character.isDigit

  /**
    * write the czml document to a file.
    * @param outFile the file name to write to, if empty or missing output will be to System.out
    * @param czml the CZML document, i.e. the list of CZML packets
    */
  def writeCzmlToFile(czml: CZML[CZMLPacket], outFile: Option[String] = None) = {
    val writer = if (outFile.isEmpty) new PrintWriter(System.out) else new PrintWriter(new File(outFile.get))
    try {
      writer.write(Json.prettyPrint(Json.toJson(czml)))
    } catch {
      case e: IOException => e.printStackTrace()
    }
    finally {
      writer.flush()
      // close files, not System.out
      if (outFile.nonEmpty) writer.close()
    }
  }

  /**
    * write the (json) string representation of a czml document to a file.
    * @param outFile the file name to write to, if empty or missing output will be to System.out
    * @param czmljs the CZML document as a (json) string
    */
  def writeJsToFile(czmljs: String, outFile: Option[String] = None) = {
    val writer = if (outFile.isEmpty) new PrintWriter(System.out) else new PrintWriter(new File(outFile.get))
    try {
      writer.write(czmljs)
    } catch {
      case e: IOException => e.printStackTrace()
    }
    finally {
      writer.flush()
      // close files, not System.out
      if (outFile.nonEmpty) writer.close()
    }
  }


}
