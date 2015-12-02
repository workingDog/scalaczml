package com.kodekutters.czml

import com.kodekutters.czmlProtocol._
import play.api.libs.json.Json

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source


/**
  * a basic example
  */
object Example1 {

  def main(args: Array[String]) {

      // read a CZML document from a file
      val jsonDoc = Source.fromFile("/......./test4.czml").mkString
      // create a czml object from the json document
     val czml = CZML(jsonDoc)
     // create one position property
     val pos = CzmlPositions(9.3, 8.2, 7.1)
     // create a billboard property
      val bb = Billboard("http://localhost/img.png",  0.7)
     // create a czml packet
     val packet = new CZMLPacket(id = Some("test packet"), version = Some("1.0"), propertyList = ListBuffer(pos, bb))
     // add the packet to the existing czml object
     czml.add(packet)  // or czml.packets += packet
     // convert the czml object to json
     val jsczml = Json.toJson(czml)
     // write the czml to file (here to System.out)
     Util.writeToFile("", czml)
     // alternatively
      //  Util.writeToFile("", Json.prettyPrint(jsczml))

  }
}