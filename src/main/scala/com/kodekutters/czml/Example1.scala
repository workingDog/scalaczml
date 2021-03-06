package com.kodekutters.czml

import com.kodekutters.czml.czmlProperties._
import com.kodekutters.czml.CzmlImplicits._

import play.api.libs.json.Json
import scala.collection.mutable.HashSet
import scala.io.Source


/**
  * a basic example
  */
object Example1 {
  def main(args: Array[String]) {
    // read a CZML document from a file
    val jsonDoc = Source.fromFile("/.....test4.czml").mkString
    // create a czml object from the json document
    val czml = CZML[CZMLPacket](jsonDoc)
    // create a position property
    val pos = CzmlPositions(9.3, 8.2, 7.1)
    // create a billboard property with image uri and scale fields
    val bb = new Billboard(image = "http://localhost/img.png", scale = 0.7)
    // create a czml packet
    val packet = new CZMLPacket("test packet", HashSet[CzmlProperty](pos, bb))
    // add the packet to the existing czml object
    czml.add(packet) // or czml.packets += packet
    // convert the czml object to json
    val jsczml = Json.toJson(czml)
    // write the czml as a json document to file (here to System.out)
    Util.writeCzmlToFile(czml)
    // alternatively
    //  Util.writeJsToFile(Json.prettyPrint(jsczml))
  }
}