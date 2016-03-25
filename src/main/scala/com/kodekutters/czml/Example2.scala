package com.kodekutters.czml

import com.kodekutters.czml.czmlProperties._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.collection.mutable
import scala.io.Source

/**
  * example of ad-hock mixing in a custom element
  *
  * NOTE: for a more principled way to add custom properties see ExampleCustom
  *
  */

/**
  * make a custom packet that extends Packet
  *
  * @param mydata     the custom property
  * @param czmlPacket the CZML standard packet
  */
case class MyPacket(mydata: Option[Array[Int]], czmlPacket: Option[CZMLPacket]) extends Packet {
  def asEventSourceData(): String = {
    val sb = new mutable.StringBuilder("data: \n")
    sb.append(Json.toJson(this) + "\n\n")
    sb.toString()
  }
}

object MyPacket {

  val theReads: Reads[MyPacket] =
    ((JsPath \ "mydata").readNullable[Array[Int]] and CZMLPacket.theReads) ((mydata, czml) => MyPacket(mydata, Option(czml)))

  val theWrites: Writes[MyPacket] =
    ((JsPath \ "mydata").writeNullable[Array[Int]] and JsPath.writeNullable[CZMLPacket]) (unlift(MyPacket.unapply))

  implicit val fmt: Format[MyPacket] = Format(theReads, theWrites)
}

object Example2 {

  def main(args: Array[String]) {
    // a file with the optional added property in the packets, e.g. "mydata": [1,2,3,4,5]
    val jsonDoc = Source.fromFile("/....../test.czml").mkString
    // read in the custom json document
    val czml = CZML[MyPacket](jsonDoc)
    println("number of MyPacket: " + czml.packets.length + "\n")
    // print out each custom packet
    for (packet <- czml.packets) {
      println("MyPacket: " + Json.prettyPrint(Json.toJson(packet)) + "\n")
    }

  }
}
