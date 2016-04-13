package com.kodekutters.czml

import java.awt.Color

import com.kodekutters.czml.CzmlImplicits._
import com.kodekutters.czml.czmlCore._
import com.kodekutters.czml.czmlProperties._
import com.kodekutters.czml.czmlCustom.{CustomBasic, CustomInterval, CustomList, CustomMap}
import play.api.libs.json.Json

import scala.collection.mutable.{HashSet, ListBuffer, ListMap}


/**
  * an example using custom properties
  */
object ExampleCustom {
  def main(args: Array[String]) {
    // create an empty czml object
    val czml = CZML[CZMLPacket]()
    // add a typical first packet
    czml.packets += new CZMLPacket(id = "document", version = "1.0")
    // create a positions property
    val pos = new CzmlPositions(new CzmlPosition(cartographicDegrees = Cartographic[DEGREE](151.12, -33.52, 123.0)))
    // create a billboard property
    val bill = new Billboard(image = "https://upload.wikimedia.org/wikipedia/commons/c/c4/PM5544_with_non-PAL_signals.png", color = Color.red, show = true, scale = 0.2)
    // create a label with some text
    val label = new Label(eyeOffset = CzmlCartesian(5, 6, 7), text = "some text here", font = "11pt Lucida Console", outlineColor = Color.orange)

    // a custom property consisting of a map of key=field name, value=various types including another map
    val customMap = new CustomMap(ListMap(
      "some-string" -> "xxxx",
      "some-int" -> 123,
      "some-map" -> Map("children" -> ListBuffer("one", "two", "three", 123))).toMap)

    // a list/map of custom property (key=field name, value=various types)
    val theList = ListMap("children" -> customMap,
      "custom-array" -> new CustomList(List(1, 2, 3, 4, 5)),
      "basic-double" -> new CustomBasic(23.4),
      "basic-string" -> new CustomBasic("some-text"),
      "change-name" -> new CustomInterval("2007-03-02T13:00:00Z/2008-05-11T15:30:00Z", "XYZ"),
      "basic-array" -> new CustomBasic(ListBuffer(9, 8, 7)))

    // create a custom properties czml property
    val custom = new CustomProperties(theList.toMap)

    // create a czml packet with all the czml properties
    val packet = new CZMLPacket("test packet", HashSet[CzmlProperty](pos, bill, label, custom))
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