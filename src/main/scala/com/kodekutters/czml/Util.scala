package com.kodekutters.czml

import java.io.{IOException, File, PrintWriter}

import com.kodekutters.czmlProtocol.CZML
import play.api.libs.json.Json


/**
  * supporting basic utilities
  */
object Util {

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
  val HORIZONTAL = "HORIZONTAL"
  val VERTICAL = "VERTICAL"
  val UNBOUNDED = "UNBOUNDED"
  val CLAMPED = "CLAMPED"
  val LOOP_STOP = "LOOP_STOP"
  val SYSTEM_CLOCK = "SYSTEM_CLOCK"
  val SYSTEM_CLOCK_MULTIPLIER = "SYSTEM_CLOCK_MULTIPLIER"
  val TICK_DEPENDENT = "TICK_DEPENDENT"


  def isEmpty(x: String) = x != null && x.trim.nonEmpty

  def isAllDigits(x: String) = x forall Character.isDigit

  def writeToFile(outFile: String = "", czml: CZML) = {
    val writer = if (outFile.trim.isEmpty) new PrintWriter(System.out) else new PrintWriter(new File(outFile))
    try {
      writer.write(Json.prettyPrint(Json.toJson(czml)))
    } catch {
      case e: IOException => e.printStackTrace()
    }
    finally {
      writer.close()
    }
  }

  def writeToFile(outFile: String, czmljs: String) = {
    val writer = if (outFile.trim.isEmpty) new PrintWriter(System.out) else new PrintWriter(new File(outFile))
    try {
      writer.write(czmljs)
    } catch {
      case e: IOException => e.printStackTrace()
    }
    finally {
      writer.close()
    }
  }

}
