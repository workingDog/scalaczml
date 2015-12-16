# ScalaCZML a CZML library in scala 

This library **ScalaCZML** reads and writes 
[CZML JSON](https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Guide) entities and 
presents them as [Scala](http://www.scala-lang.org/) objects.

[Cesium](http://cesiumjs.org/) is a JavaScript library for creating 3D globes and 2D maps in a web browser without a plugin. 
It uses WebGL for hardware-accelerated graphics, and is cross-platform, cross-browser, 
and tuned for dynamic-data visualization. 

[CZML](https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Guide) is a JSON schema for 
describing a time-dynamic graphical scene, primarily for display in a web browser running Cesium.
It describes lines, points, billboards (markers), models and
other graphical primitives, and specifies how they change with time.

**ScalaCZML** makes the CZML JSON entities available as Scala objects. 
The library follows the specification of reference 1, but also include additional objects such as Rectangle and Wall.

## References
 
1) [CZML guide](https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Guide)

2) [CZML content](https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Content)

3) [CZML structure](https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Structure)

## Dependencies

Using [Scala 2.11](http://www.scala-lang.org/) and the Play-JSON component 
of the [Play Framework](https://www.playframework.com/)

See also the build.sbt file.

## Compiling and packaging

Using [SBT](http://www.scala-sbt.org/), just type "sbt package" to generate a jar file that you can then 
include in your project. The jar file will be in the directory:
 ".../target/scala-2.11/scalaczml_2.11-1.0.jar" 

## Documentation

To generate the ScalaCZML API documentation, type "sbt doc". The documentation will be generated in 
/target/scala-2.11/api directory. **ScalaCZML** mostly follows reference 2 documentation. 

## Example use

    object Example1 {
      def main(args: Array[String]) {
        // read a CZML document from a file
        val jsonDoc = Source.fromFile("/......./test4.czml").mkString
        // create a czml object from the json document
        val czml = CZML[CZMLPacket](jsonDoc)
        // create a position property
        val pos = CzmlPositions(9.3, 8.2, 7.1)
        // create a billboard property with image uri and scale fields
        val bb = Billboard("http://localhost/img.png", 0.7)
        // create a czml packet
        val packet = new CZMLPacket("test packet", ListBuffer[CzmlProperty](pos, bb))
        // add the packet to the existing czml object
        czml.add(packet) // or czml.packets += packet
        // convert the czml object to json
        val jsczml = Json.toJson(czml)
        // write the czml to file (here to System.out)
        Util.writeToFile("", czml)
        // alternatively
        //  Util.writeToFile("", Json.prettyPrint(jsczml))
      }
    }
    
## Status

ongoing

