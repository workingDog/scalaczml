package com.kodekutters.czml

import com.kodekutters.czml.czmlProperties._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.collection.mutable
import scala.io.Source

/**
  * example of mixing in a custom property
  */

/**
  * make a custom packet that extends Packet
  *
  * @param mydata     the custom property
  * @param czmlPacket the CZML standard packet
  */
case class MyPacket(mydata: Option[Array[Int]], czmlPacket: Option[CZMLPacket]) extends Packet {
  def asEventSource(): String = {
    val sb = new mutable.StringBuilder("event: czml \n data: ")
    sb.append(Json.prettyPrint(Json.toJson(this)) + "\n")
    sb.toString()
  }
}

object MyPacket {

  val theReads = new Reads[MyPacket] {
    def reads(js: JsValue): JsResult[MyPacket] = {
      val mydata = (JsPath \ "mydata").read[Array[Int]].reads(js).asOpt
      val czmlPacket = CZMLPacket.fmt.reads(js).asOpt
      JsSuccess(new MyPacket(mydata, czmlPacket))
    }
  }

  val theWrites: Writes[MyPacket] =
    ((JsPath \ "mydata").writeNullable[Array[Int]] and JsPath.writeNullable[CZMLPacket]) (unlift(MyPacket.unapply))

  implicit val fmt: Format[MyPacket] = Format(theReads, theWrites)
}

object Example2 {

  def main(args: Array[String]) {
    // a file with the optional added property in the packets, e.g. "mydata": [1,2,3,4,5]
    val jsonDoc = Source.fromFile("....../test5.czml").mkString
    // read in the custom json document
    val czml = CZML[MyPacket](jsonDoc)
    println("number of MyPacket: " + czml.packets.length + "\n")
    // print out each custom packet
    for (packet <- czml.packets) {
      println("MyPacket: " + Json.prettyPrint(Json.toJson(packet)) + "\n")
    }

  }
}
