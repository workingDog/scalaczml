# ScalaCZML a CZML library in scala 

This library **ScalaCZML** reads and writes [CZML JSON](https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/CZML-Guide) entities and presents them as [Scala](http://www.scala-lang.org/) objects.

[Cesium](http://cesiumjs.org/) is a JavaScript library for creating 3D globes and 2D maps in a web browser without a plugin. 
It uses WebGL for hardware-accelerated graphics, and is cross-platform, cross-browser, 
and tuned for dynamic-data visualization. 

[CZML](https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/CZML-Guide) is a JSON schema for describing a time-dynamic graphical scene, primarily for display in a web browser running Cesium.
It describes lines, points, billboards (markers), models and other graphical primitives, and specifies how they change with time.

**ScalaCZML** makes the CZML JSON entities available as Scala objects. The library follows the specifications of reference 1. 
Typically **ScalaCZML** is for server side application development.

There is also the Cesium library in Scala, [CesiumScala](https://github.com/workingDog/CesiumScala) for client side development.

## References
 
1) [CZML guide](https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/CZML-Guide)

2) [CZML content](https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/Packet)

3) [CZML structure](https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/CZML-Structure)

## Dependencies

Using [Scala 2.11](http://www.scala-lang.org/) and the Play-JSON component 
of the [Play Framework](https://www.playframework.com/)

See also the build.sbt file.

## Installation and packaging

Add the following dependency to build.sbt:

    libraryDependencies += "com.github.workingDog" %% "scalaczml" % "0.2"

To compile and generate a jar file from the source:

    sbt package

The jar file (scalaczml_2.11-0.3-SNAPSHOT.jar) will be in the "./target/scala-2.11/" directory.

## Documentation

To generate the ScalaCZML API documentation, type "sbt doc". The documentation will be generated in 
/target/scala-2.11/api directory. **ScalaCZML** mostly follows reference 2 documentation. 

## Example

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
    
## Status

usable, ongoing work

