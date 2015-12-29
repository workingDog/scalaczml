package com.kodekutters.czml

import java.awt.Color

import com.kodekutters.czml.czmlProperties._
import com.kodekutters.czml.czmlCore._
import com.kodekutters.czml.CzmlImplicits._
import play.api.libs.json.Json

import scala.collection.mutable.HashSet


/**
  * Example using CzmlImplicits
  */
object Example3 {
  def main(args: Array[String]) {
    // create an empty czml object
    val czml = CZML[CZMLPacket]()
    // add a typical first packet
    czml.packets += new CZMLPacket(id = "document", version = "1.0")
    // create a positions property
    val pos = new CzmlPositions(new CzmlPosition(cartographicDegrees = Cartographic[DEGREE](151.12, -33.52, 123.0)))
    // create a billboard property
    val bb = new Billboard(image = "https://upload.wikimedia.org/wikipedia/commons/c/c4/PM5544_with_non-PAL_signals.png", color = Color.red, show = true, scale = 0.2)
    // create a label with some text
    val label = new Label(eyeOffset = CzmlCartesian(5, 6, 7), text = "some text here", font = "11pt Lucida Console", outlineColor = Color.orange)
    // create some positions for the polygon
    val polyPos = Array(Position(1, 2, 3), Position(4, 5, 6), Position(7, 8, 9))
    // create a polygon
    val poly = new Polygon(positions = polyPos, material = Material(Color.blue))
    // create a czml packet with all the czml properties
    val packet = new CZMLPacket("test packet", HashSet[CzmlProperty](pos, bb, label, poly))
    // add the packet to the czml object
    czml.packets += packet
    // convert the czml object to json
    val jsczml = Json.toJson(czml)
    // print the json representation
    println(Json.prettyPrint(jsczml))
    // alternatively, write the czml (as json) directly to file (here to System.out)
    //  Util.writeCzmlToFile(czml)
  }
}