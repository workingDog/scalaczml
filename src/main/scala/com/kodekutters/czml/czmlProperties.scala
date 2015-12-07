/*
 * Copyright (c) 2015, Ringo Wathelet
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 * - Neither the name of "scalaczml" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.kodekutters.czml

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import com.kodekutters.czml.czmlCore._

/**
  * The Cesium CZML language as described in the following references:
  *
  * https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Structure
  * https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Content
  * https://github.com/AnalyticalGraphicsInc/cesium/wiki/CZML-Guide
  *
  * CZML is a JSON schema for describing a time-dynamic graphical scene,
  * primarily for display in a web browser running Cesium.
  * It describes lines, points, billboards (markers), models and
  * other graphical primitives, and specifies how they change with time.
  *
  * This package implements the CZML object as a list of CzmlPackage,
  * the CZMLPackage object and all its constituent czml properties.
  */
package object czmlProperties {

  sealed trait CzmlProperty

  /**
    * representing time availability as a String or an array of strings
    *
    * @param value the availability value(s)
    */
  final case class Availability(value: Either[String, Array[String]]) extends CzmlProperty {
    def this(value: String) = this(Left(value))

    def this(values: Array[String]) = this(Right(values))
  }

  object Availability {

    def apply(value: String): Availability = new Availability(value)

    def apply(values: Array[String]): Availability = new Availability(values)

    val theReads = new Reads[Availability] {
      def reads(json: JsValue): JsResult[Availability] = {
        (JsPath.read[String].map(Availability(_)) | JsPath.read[Array[String]].map(Availability(_))).reads(json)
      }
    }

    val theWrites = new Writes[Availability] {
      def writes(time: Availability) = {
        time.value match {
          case Left(x) => JsString(x)
          case Right(x) => Json.toJson(x)
        }
      }
    }

    implicit val fmt: Format[Availability] = Format(theReads, theWrites)
  }

  /**
    * A billboard, or viewport-aligned image. The billboard is positioned in the scene by the position property.
    * A billboard is sometimes called a marker.
    */
  final case class Billboard(color: Option[ColorProperty] = None, eyeOffset: Option[EyeOffset] = None,
                             horizontalOrigin: Option[HorizontalOrigin] = None, image: Option[ImageUri] = None,
                             pixelOffset: Option[PixelOffset] = None, scale: Option[Number] = None,
                             rotation: Option[Number] = None, alignedAxis: Option[AlignedAxis] = None,
                             show: Option[CzmlBoolean] = None, verticalOrigin: Option[VerticalOrigin] = None) extends CzmlProperty {

    def this(color: ColorProperty) = this(color = Option(color), show = Option(CzmlBoolean(true)))

    def this(color: CzmlColor) = this(color = Option(ColorProperty(color)), show = Option(CzmlBoolean(true)))

    def this(rgba: Rgba) = this(color = Option(ColorProperty(rgba)), show = Option(CzmlBoolean(true)))

    def this(r: Int, g: Int, b: Int, a: Int) = this(Option(ColorProperty(r, g, b, a)), show = Option(CzmlBoolean(true)))

    def this(rgbaf: Rgbaf) = this(color = Option(ColorProperty(rgbaf)), show = Option(CzmlBoolean(true)))

    def this(r: Float, g: Float, b: Float, a: Float) = this(Option(ColorProperty(r, g, b, a)), show = Option(CzmlBoolean(true)))

    def this(r: Double, g: Double, b: Double, a: Double) = this(Option(ColorProperty(r, g, b, a)), show = Option(CzmlBoolean(true)))

    def this(image: ImageUri) = this(image = Option(image), show = Option(CzmlBoolean(true)))

    def this(uri: String) = this(image = Option(ImageUri(uri)), show = Option(CzmlBoolean(true)))

    def this(uri: String, scale: Double) = this(image = Option(ImageUri(uri)), scale = Option(Number(scale)), show = Option(CzmlBoolean(true)))

    def this(image: ImageUri, scale: Double) = this(image = Option(image), scale = Option(Number(scale)), show = Option(CzmlBoolean(true)))

    def this(image: ImageUri, scale: Double, rotation: Double) = this(image = Option(image), scale = Option(Number(scale)),
      rotation = Option(Number(rotation)), show = Option(CzmlBoolean(true)))
  }

  object Billboard {
    implicit val fmt = Json.format[Billboard]

    def apply(color: ColorProperty): Billboard = new Billboard(color)

    def apply(color: CzmlColor): Billboard = new Billboard(color)

    def apply(rgba: Rgba): Billboard = new Billboard(rgba)

    def apply(r: Int, g: Int, b: Int, a: Int): Billboard = new Billboard(r, g, b, a)

    def apply(rgbaf: Rgbaf): Billboard = new Billboard(rgbaf)

    def apply(r: Float, g: Float, b: Float, a: Float): Billboard = new Billboard(r, g, b, a)

    def apply(r: Double, g: Double, b: Double, a: Double): Billboard = new Billboard(r, g, b, a)

    def apply(image: ImageUri): Billboard = new Billboard(image)

    def apply(image: ImageUri, scale: Double): Billboard = new Billboard(image, scale)

    def apply(uri: String, scale: Double): Billboard = new Billboard(image = ImageUri(uri), scale)

    def apply(image: ImageUri, scale: Double, rotation: Double): Billboard = new Billboard(image, scale, rotation)

  }

  /**
    * The orientation of the object in the world. The orientation has no direct visual representation,
    * but it is used to orient models, cones, and pyramids attached to the object.
    */
  final case class Orientation(axes: Option[String] = None, unitQuaternion: Option[Array[Double]] = None,
                               reference: Option[String] = None, epoch: Option[String] = None,
                               nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                               interpolationAlgorithm: Option[String] = None,
                               interpolationDegree: Option[Int] = None,
                               forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                               backwardExtrapolationType: Option[String] = None,
                               backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable with CzmlProperty {

    def this(axes: String) = this(Option(axes))

    def this(unitQuaternion: Array[Double]) = this(None, Option(unitQuaternion))

  }

  object Orientation {
    implicit val fmt = Json.format[Orientation]

    def apply(axes: String): Orientation = new Orientation(axes)

    def apply(unitQuaternion: Array[Double]): Orientation = new Orientation(unitQuaternion)
  }

  /**
    * A point, or viewport-aligned circle. The point is positioned in the scene by the position property.
    */
  final case class Point(color: Option[ColorProperty] = None, outlineColor: Option[ColorProperty] = None,
                         outlineWidth: Option[Number] = None, pixelSize: Option[Number] = None,
                         show: Option[CzmlBoolean] = None) extends CzmlProperty {

    def this(color: ColorProperty, outlineColor: ColorProperty, outlineWidth: Number, pixelSize: Number) =
      this(Option(color), Option(outlineColor), Option(outlineWidth), Option(pixelSize), Option(new CzmlBoolean(true)))

    def this(color: CzmlColor, outlineColor: CzmlColor, outlineWidth: Double, pixelSize: Double) =
      this(Option(new ColorProperty(color)), Option(new ColorProperty(outlineColor)),
        Option(new Number(outlineWidth)), Option(new Number(pixelSize)), Option(new CzmlBoolean(true)))
  }

  object Point {
    implicit val fmt = Json.format[Point]

    def apply(color: ColorProperty, outlineColor: ColorProperty, outlineWidth: Number, pixelSize: Number): Point =
      new Point(color, outlineColor, outlineWidth, pixelSize)

    def apply(color: CzmlColor, outlineColor: CzmlColor, outlineWidth: Double, pixelSize: Double): Point =
      new Point(color, outlineColor, outlineWidth, pixelSize)

  }

  /**
    * A string of text. The label is positioned in the scene by the position property.
    */
  final case class Label(eyeOffset: Option[EyeOffset] = None, fillColor: Option[ColorProperty] = None, font: Option[Font] = None,
                         horizontalOrigin: Option[HorizontalOrigin] = None, outlineColor: Option[ColorProperty] = None,
                         outlineWidth: Option[Number] = None, pixelOffset: Option[PixelOffset] = None,
                         scale: Option[Number] = None, show: Option[CzmlBoolean] = None, style: Option[Style] = None,
                         text: Option[Text] = None, verticalOrigin: Option[VerticalOrigin] = None) extends CzmlProperty {

    def this(text: String) = this(text = Option(Text(text)), show = Option(CzmlBoolean(true)))

    def this(text: String, font: String) = this(text = Option(Text(text)), font = Option(Font(font)), show = Option(CzmlBoolean(true)))
  }

  object Label {
    implicit val fmt = Json.format[Label]

    def apply(text: String): Label = new Label(text)

    def apply(text: String, font: String): Label = new Label(text, font)

  }

  /**
    * An array of CzmlPosition. This CzmlPositions property is used only in CZMLPacket.
    *
    * @param values the array of CzmlPosition
    */
  final case class CzmlPositions(values: Option[Array[CzmlPosition]]) extends CzmlProperty {
    def this(values: Array[CzmlPosition]) = this(Option(values))

    def this(position: CzmlPosition) = this(Option(Array(position)))

    def this(referenceFrame: String, cartesian: Cartesian, interval: String) =
      this(CzmlPosition(referenceFrame, cartesian, interval))

    def this(referenceFrame: String, x: Double, y: Double, z: Double, interval: String) =
      this(CzmlPosition(referenceFrame, x, y, z, interval))


    def this(x: Double, y: Double, z: Double) = this(CzmlPosition(x, y, z))

    def this(referenceFrame: String, x: Double, y: Double, z: Double) = this(CzmlPosition(referenceFrame, x, y, z))

    def this(cartographicDegrees: Cartographic) = this(CzmlPosition(cartographicDegrees))

    def this(lngLatAltT: LngLatAltT) = this(CzmlPosition(lngLatAltT))

  }

  object CzmlPositions {

    def apply(values: Array[CzmlPosition]): CzmlPositions = new CzmlPositions(Option(values))

    def apply(position: CzmlPosition): CzmlPositions = new CzmlPositions(Option(Array(position)))

    def apply(referenceFrame: String, cartesian: Cartesian, interval: String): CzmlPositions = new CzmlPositions(referenceFrame, cartesian, interval)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double, interval: String): CzmlPositions = new CzmlPositions(referenceFrame, x, y, z, interval)

    def apply(x: Double, y: Double, z: Double): CzmlPositions = new CzmlPositions(x, y, z)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double): CzmlPositions = new CzmlPositions(referenceFrame, x, y, z)

    def apply(cartographicDegrees: Cartographic): CzmlPositions = new CzmlPositions(cartographicDegrees)

    def apply(lngLatAltT: LngLatAltT): CzmlPositions = new CzmlPositions(lngLatAltT)

    val theReads = new Reads[CzmlPositions] {
      def reads(js: JsValue): JsResult[CzmlPositions] = {
        JsPath.read[Array[CzmlPosition]].reads(js).asOpt match {
          // have a single property that we wrap in an option array
          case None => JsSuccess(new CzmlPositions(Option(Array(JsPath.read[CzmlPosition].reads(js).getOrElse(new CzmlPosition())))))
          // have an array of properties
          case Some(b) => JsSuccess(new CzmlPositions(Some(b)))
        }
      }
    }

    val theWrites = new Writes[CzmlPositions] {
      def writes(propList: CzmlPositions) = {
        propList.values match {
          case None => JsNull
          case Some(list) =>
            if (list.length == 1) CzmlPosition.fmt.writes(list.head)
            else Json.toJson(list.asInstanceOf[Array[CzmlPosition]])
        }
      }
    }

    implicit val fmt: Format[CzmlPositions] = Format(theReads, theWrites)
  }

  /**
    * A path, which is a polyline defined by the motion of an object over time.
    * The possible vertices of the path are specified by the position property.
    */
  final case class Path(show: Option[CzmlBoolean] = None, material: Option[LineMaterial] = None,
                        width: Option[Number] = None, resolution: Option[Number] = None,
                        leadTime: Option[Number] = None, trailTime: Option[Number] = None) extends CzmlProperty {

    def this(material: LineMaterial, width: Number, resolution: Number) =
      this(Option(CzmlBoolean(true)), Option(material), Option(width), Option(resolution))

    def this(material: LineMaterial, width: Double, resolution: Double) =
      this(Option(CzmlBoolean(true)), Option(material), Option(new Number(width)), Option(new Number(resolution)))
  }

  object Path {
    implicit val fmt = Json.format[Path]

    def apply(material: LineMaterial, width: Number, resolution: Number): Path = new Path(material, width, resolution)

    def apply(material: LineMaterial, width: Double, resolution: Double): Path = new Path(material, width, resolution)

  }

  /**
    * A polyline, which is a line in the scene composed of multiple segments.
    */
  final case class Polyline(positions: Option[Positions] = None, show: Option[CzmlBoolean] = None,
                            material: Option[LineMaterial] = None, width: Option[Number] = None,
                            followSurface: Option[CzmlBoolean] = None) extends CzmlProperty {

    def this(positions: Positions, material: LineMaterial, width: Number, followSurface: Boolean) =
      this(Option(positions), Option(CzmlBoolean(true)), Option(material), Option(width), Option(CzmlBoolean(followSurface)))

    def this(positions: Position, material: LineMaterial, width: Double, followSurface: Boolean) =
      this(Option(new Positions(positions)), Option(CzmlBoolean(true)),
        Option(material), Option(new Number(width)), Option(CzmlBoolean(followSurface)))
  }

  object Polyline {
    implicit val fmt = Json.format[Polyline]

    def apply(positions: Positions, material: LineMaterial, width: Number, followSurface: Boolean): Polyline =
      new Polyline(positions, material, width, followSurface)

    def apply(positions: Position, material: LineMaterial, width: Double, followSurface: Boolean): Polyline =
      new Polyline(positions, material, width, followSurface)

  }

  /**
    * A rectangle
    */
  final case class Rectangle(coordinates: Option[WsenDegrees] = None, show: Option[CzmlBoolean] = None,
                             material: Option[Material] = None, height: Option[Number] = None,
                             extrudedHeight: Option[Number] = None, granularity: Option[Number] = None,
                             rotation: Option[Number] = None,
                             stRotation: Option[Number] = None, fill: Option[CzmlBoolean] = None,
                             outline: Option[CzmlBoolean] = None, outlineColor: Option[ColorProperty] = None,
                             outlineWidth: Option[Number] = None,
                             closeBottom: Option[CzmlBoolean] = None, closeTop: Option[CzmlBoolean] = None) extends CzmlProperty

  object Rectangle {
    implicit val fmt = Json.format[Rectangle]
  }

  /**
    * A wall
    */
  final case class Wall(positions: Option[Positions] = None, show: Option[CzmlBoolean] = None,
                        material: Option[Material] = None,
                        minimumHeights: Option[Array[Double]] = None,
                        maximumHeights: Option[Array[Double]] = None,
                        granularity: Option[Number] = None,
                        fill: Option[CzmlBoolean] = None,
                        outline: Option[CzmlBoolean] = None, outlineColor: Option[ColorProperty] = None,
                        outlineWidth: Option[Number] = None) extends CzmlProperty

  object Wall {
    implicit val fmt = Json.format[Wall]
  }

  /**
    * A polygon, which is a closed shape on the surface of the Earth.
    */
  final case class Polygon(positions: Option[Positions] = None, show: Option[CzmlBoolean] = None,
                           material: Option[Material] = None, height: Option[Number] = None,
                           extrudedHeight: Option[Number] = None, granularity: Option[Number] = None,
                           stRotation: Option[Number] = None, fill: Option[CzmlBoolean] = None,
                           outline: Option[CzmlBoolean] = None, outlineColor: Option[ColorProperty] = None,
                           perPositionHeight: Option[CzmlBoolean] = None) extends CzmlProperty {

    def this(positions: Positions, material: Material, height: Double, extrudedHeight: Double) =
      this(Option(positions), Option(CzmlBoolean(true)), Option(material), Option(Number(height)), Option(Number(extrudedHeight)))

    def this(positions: Position, material: Material, height: Double, extrudedHeight: Double) =
      this(Option(Positions(positions)), Option(CzmlBoolean(true)), Option(material), Option(Number(height)), Option(Number(extrudedHeight)))

  }

  object Polygon {
    implicit val fmt = Json.format[Polygon]

    def apply(positions: Positions, material: Material, height: Double, extrudedHeight: Double): Polygon =
      new Polygon(positions, material, height, extrudedHeight)

    def apply(positions: Position, material: Material, height: Double, extrudedHeight: Double): Polygon =
      new Polygon(positions, material, height, extrudedHeight)
  }

  /**
    * The dimensions of the ellipsoid. Also describes the viewFrom property.
    */
  final case class Radii(cartesian: Option[Cartesian] = None, interval: Option[String] = None,
                         reference: Option[String] = None, epoch: Option[String] = None,
                         nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                         interpolationAlgorithm: Option[String] = None,
                         interpolationDegree: Option[Int] = None,
                         forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                         backwardExtrapolationType: Option[String] = None,
                         backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable with CzmlProperty {

    def this(cartesian: Cartesian) = this(Option(cartesian))

    def this(x: Double, y: Double, z: Double) = this(Option(new Cartesian(x, y, z)))
  }

  object Radii {
    implicit val fmt = Json.format[Radii]

    def apply(cartesian: Cartesian): Radii = new Radii(cartesian)

    def apply(x: Double, y: Double, z: Double): Radii = new Radii(x, y, z)
  }

  /**
    * An ellipsoid, which is a closed quadric surface that is a three dimensional analogue of an ellipse.
    * The ellipsoid is positioned and oriented using the position and orientation properties.
    */
  final case class Ellipsoid(show: Option[CzmlBoolean] = None, radii: Option[Radii] = None,
                             fill: Option[CzmlBoolean] = None, material: Option[Material] = None,
                             outline: Option[CzmlBoolean] = None, outlineColor: Option[ColorProperty] = None,
                             stackPartitions: Option[Number] = None, slicePartitions: Option[Number] = None,
                             subdivisions: Option[Number] = None) extends CzmlProperty {

    def this(x: Double, y: Double, z: Double, fill: Boolean, material: Material,
             outline: Boolean, outlineColor: ColorProperty, stackPartitions: Double, slicePartitions: Double,
             subdivisions: Double) = this(Option(CzmlBoolean(true)), Option(Radii(x, y, z)), Option(CzmlBoolean(fill)),
      Option(material), Option(CzmlBoolean(outline)), Option(outlineColor), Option(Number(stackPartitions)),
      Option(Number(slicePartitions)), Option(Number(subdivisions)))

  }

  object Ellipsoid {
    implicit val fmt = Json.format[Ellipsoid]

    def apply(x: Double, y: Double, z: Double,
              fill: Boolean, material: Material,
              outline: Boolean, outlineColor: ColorProperty,
              stackPartitions: Double, slicePartitions: Double,
              subdivisions: Double): Ellipsoid = new Ellipsoid(x, y, z, fill, material, outline, outlineColor,
      stackPartitions, slicePartitions, subdivisions)
  }

  /**
    * An ellipse, which is a closed curve on the surface of the Earth.
    * The ellipse is positioned using the position property.
    */
  final case class Ellipse(show: Option[CzmlBoolean] = None, semiMajorAxis: Option[Number] = None, semiMinorAxis: Option[Number] = None,
                           rotation: Option[Number] = None, material: Option[Material] = None,
                           height: Option[Number] = None, extrudedHeight: Option[Number] = None,
                           granularity: Option[Number] = None, stRotation: Option[Number] = None,
                           fill: Option[CzmlBoolean] = None, outline: Option[CzmlBoolean] = None,
                           outlineColor: Option[ColorProperty] = None,
                           numberOfVerticalLines: Option[Number] = None) extends CzmlProperty {

    def this(semiMajorAxis: Double, semiMinorAxis: Double, rotation: Double,
             material: Material, height: Double, extrudedHeight: Double, granularity: Double,
             stRotation: Double, fill: Boolean, outline: Boolean, outlineColor: CzmlColor, numberOfVerticalLines: Double) =
      this(Option(CzmlBoolean(true)), Option(Number(semiMajorAxis)), Option(Number(semiMinorAxis)),
        Option(Number(rotation)), Option(material),
        Option(Number(height)), Option(Number(extrudedHeight)),
        Option(Number(granularity)), Option(Number(stRotation)), Option(CzmlBoolean(fill)),
        Option(CzmlBoolean(outline)), Option(ColorProperty(outlineColor)), Option(Number(numberOfVerticalLines)))

  }

  object Ellipse {
    implicit val fmt = Json.format[Ellipse]

    def apply(semiMajorAxis: Double, semiMinorAxis: Double, rotation: Double,
              material: Material, height: Double, extrudedHeight: Double, granularity: Double,
              stRotation: Double, fill: Boolean, outline: Boolean, outlineColor: CzmlColor, numberOfVerticalLines: Double): Ellipse =
      new Ellipse(semiMajorAxis, semiMinorAxis, rotation, material, height, extrudedHeight, granularity,
        stRotation, fill, outline, outlineColor, numberOfVerticalLines)
  }

  /**
    * A conical sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    */
  final case class ConicSensor(show: Option[CzmlBoolean] = None, innerHalfAngle: Option[Number] = None,
                               outerHalfAngle: Option[Number] = None, minimumClockAngle: Option[Number] = None,
                               maximumClockAngle: Option[Number] = None, radius: Option[Number] = None,
                               showIntersection: Option[CzmlBoolean] = None, intersectionColor: Option[ColorProperty] = None,
                               intersectionWidth: Option[Number] = None, showLateralSurfaces: Option[CzmlBoolean] = None,
                               lateralSurfaceMaterial: Option[Material] = None, showEllipsoidSurfaces: Option[CzmlBoolean] = None,
                               ellipsoidSurfaceMaterial: Option[Material] = None,
                               showEllipsoidHorizonSurfaces: Option[CzmlBoolean] = None,
                               ellipsoidHorizonSurfaceMaterial: Option[Material] = None,
                               showDomeSurfaces: Option[CzmlBoolean] = None, domeSurfaceMaterial: Option[Material] = None,
                               portionToDisplay: Option[PortionToDisplay] = None) extends CzmlProperty

  object ConicSensor {
    implicit val fmt = Json.format[ConicSensor]
  }

  /**
    * A custom sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    */
  final case class CustomPatternSensor(show: Option[CzmlBoolean] = None, directions: Option[Directions] = None,
                                       radius: Option[Number] = None, showIntersection: Option[CzmlBoolean] = None,
                                       intersectionColor: Option[ColorProperty] = None, intersectionWidth: Option[Number] = None,
                                       showLateralSurfaces: Option[CzmlBoolean] = None, lateralSurfaceMaterial: Option[Material] = None,
                                       showEllipsoidHorizonSurfaces: Option[CzmlBoolean] = None, ellipsoidHorizonSurfaceMaterial: Option[Material] = None,
                                       showDomeSurfaces: Option[CzmlBoolean] = None, domeSurfaceMaterial: Option[Material] = None,
                                       portionToDisplay: Option[PortionToDisplay] = None) extends CzmlProperty

  object CustomPatternSensor {
    implicit val fmt = Json.format[CustomPatternSensor]
  }

  /**
    * Defines a fan, which starts at a point or apex and extends in a specified list of directions from the apex.
    * Each pair of directions forms a face of the fan extending to the specified radius.
    */
  final case class Fan(show: Option[CzmlBoolean] = None,
                       directions: Option[Directions] = None, radius: Option[Number] = None,
                       perDirectionRadius: Option[CzmlBoolean] = None, material: Option[Material] = None,
                       fill: Option[CzmlBoolean] = None, outline: Option[CzmlBoolean] = None,
                       numberOfRings: Option[Number] = None, outlineColor: Option[ColorProperty] = None) extends CzmlProperty

  object Fan {
    implicit val fmt = Json.format[Fan]
  }

  /**
    * A rectangular pyramid sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    */
  final case class RectangularSensor(show: Option[CzmlBoolean] = None, xHalfAngle: Option[Number] = None,
                                     yHalfAngle: Option[Number] = None,
                                     radius: Option[Number] = None,
                                     showIntersection: Option[CzmlBoolean] = None,
                                     intersectionColor: Option[ColorProperty] = None,
                                     intersectionWidth: Option[Number] = None,
                                     showLateralSurfaces: Option[CzmlBoolean] = None,
                                     lateralSurfaceMaterial: Option[Material] = None,
                                     showEllipsoidSurfaces: Option[CzmlBoolean] = None,
                                     ellipsoidSurfaceMaterial: Option[Material] = None,
                                     showEllipsoidHorizonSurfaces: Option[CzmlBoolean] = None,
                                     ellipsoidHorizonSurfaceMaterial: Option[Material] = None,
                                     showDomeSurfaces: Option[CzmlBoolean] = None,
                                     domeSurfaceMaterial: Option[Material] = None,
                                     portionToDisplay: Option[PortionToDisplay] = None) extends CzmlProperty

  object RectangularSensor {
    implicit val fmt = Json.format[RectangularSensor]
  }

  /**
    * Defines a graphical vector that originates at the position property and
    * extends in the provided direction for the provided length.
    */
  final case class AgiVector(show: Option[CzmlBoolean] = None, color: Option[ColorProperty] = None,
                             direction: Option[Directions] = None, length: Option[Number] = None,
                             minimumLengthInPixels: Option[Number] = None) extends CzmlProperty {

    def this(show: CzmlBoolean, color: ColorProperty, direction: Directions, length: Number, minimumLengthInPixels: Number) =
      this(Option(show), Option(color), Option(direction), Option(length), Option(minimumLengthInPixels))
  }

  object AgiVector {
    implicit val fmt = Json.format[AgiVector]
  }

  /**
    * The clock settings for the entire data set. Only valid on the document object.
    */
  final case class Clock(currentTime: Option[String] = None, multiplier: Option[Double] = None,
                         range: Option[String] = None, step: Option[String] = None, interval: Option[String] = None) extends CzmlProperty {

    def this(currentTime: String, multiplier: Double, range: String, step: String, interval: String) =
      this(Option(currentTime), Option(multiplier), Option(range), Option(step), Option(interval))
  }

  object Clock {
    implicit val fmt = Json.format[Clock]

    def apply(currentTime: String, multiplier: Double, range: String, step: String, interval: String): Clock =
      new Clock(currentTime, multiplier, range, step, interval)

  }

  /**
    * A 3D model. The model is positioned and oriented using the position and orientation properties.
    */
  final case class Model(show: Option[CzmlBoolean] = None, scale: Option[Number] = None,
                         minimumPixelSize: Option[Number] = None, gltf: Option[ImageUri] = None,
                         runAnimations: Option[CzmlBoolean] = None,
                         nodeTransformations: Option[NodeTransformations] = None) extends CzmlProperty {

    def this(scale: Double, minimumPixelSize: Double, gltf: String) =
      this(Option(CzmlBoolean(true)), Option(new Number(scale)),
        Option(new Number(minimumPixelSize)), Option(new ImageUri(gltf)))

  }

  object Model {
    implicit val fmt = Json.format[Model]

    def apply(scale: Double, minimumPixelSize: Double, gltf: String): Model = new Model(scale, minimumPixelSize, gltf)

  }

  /**
    * A CZML packet describes the graphical properties for a single
    * object in the scene, such as a single aircraft.
    *
    */
  final case class CZMLPacket(id: Option[String] = None, name: Option[String] = None, parent: Option[String] = None,
                              description: Option[String] = None, version: Option[String] = None,
                              propertyList: ListBuffer[CzmlProperty] = ListBuffer.empty) {

    def this(id: String, name: String, parent: String, description: String, version: String, propertyList: ListBuffer[CzmlProperty]) =
      this(Option(id), Option(name), Option(parent), Option(description), Option(version), propertyList)

    def this(id: String, name: String, propertyList: ListBuffer[CzmlProperty]) =
      this(Option(id), Option(name), None, None, None, propertyList)

    def this(id: String, name: String, description: String, propertyList: ListBuffer[CzmlProperty]) =
      this(Option(id), Option(name), None, Option(description), None, propertyList)

    def this(id: String, propertyList: ListBuffer[CzmlProperty]) = this(Option(id), None, None, None, None, propertyList)

    /**
      * returns an eventSource representation of this packet
      */
    def asEventSource(): String = {
      val sb = new mutable.StringBuilder("event: czml \n data: ")
      sb.append(Json.prettyPrint(Json.toJson(this)) + "\n")
      sb.toString()
    }
  }

  object CZMLPacket {

    val theReads = new Reads[CZMLPacket] {
      def reads(js: JsValue): JsResult[CZMLPacket] = {

        val id = (JsPath \ "id").read[String].reads(js).asOpt
        val name = (JsPath \ "name").read[String].reads(js).asOpt
        val parent = (JsPath \ "parent").read[String].reads(js).asOpt
        val description = (JsPath \ "description").read[String].reads(js).asOpt
        val version = (JsPath \ "version").read[String].reads(js).asOpt

        val propList = new ListBuffer[CzmlProperty]()

        (JsPath \ "availability").read[Availability].reads(js).asOpt.map(propList += _)
        (JsPath \ "position").read[CzmlPositions].reads(js).asOpt.map(propList += _)
        (JsPath \ "billboard").read[Billboard].reads(js).asOpt.map(propList += _)
        (JsPath \ "orientation").read[Orientation].reads(js).asOpt.map(propList += _)
        (JsPath \ "point").read[Point].reads(js).asOpt.map(propList += _)
        (JsPath \ "label").read[Label].reads(js).asOpt.map(propList += _)
        (JsPath \ "path").read[Path].reads(js).asOpt.map(propList += _)
        (JsPath \ "polyline").read[Polyline].reads(js).asOpt.map(propList += _)
        (JsPath \ "polygon").read[Polygon].reads(js).asOpt.map(propList += _)
        (JsPath \ "ellipsoid").read[Ellipsoid].reads(js).asOpt.map(propList += _)
        (JsPath \ "viewFrom").read[Radii].reads(js).asOpt.map(propList += _)
        (JsPath \ "rectangle").read[Rectangle].reads(js).asOpt.map(propList += _)
        (JsPath \ "wall").read[Wall].reads(js).asOpt.map(propList += _)
        (JsPath \ "model").read[Model].reads(js).asOpt.map(propList += _)
        (JsPath \ "ellipse").read[Ellipse].reads(js).asOpt.map(propList += _)
        (JsPath \ "clock").read[Clock].reads(js).asOpt.map(propList += _)

        (JsPath \ "agi_conicSensor").read[ConicSensor].reads(js).asOpt.map(propList += _)
        (JsPath \ "agi_customPatternSensor").read[CustomPatternSensor].reads(js).asOpt.map(propList += _)
        (JsPath \ "agi_fan").read[Fan].reads(js).asOpt.map(propList += _)
        (JsPath \ "agi_rectangularSensor").read[RectangularSensor].reads(js).asOpt.map(propList += _)
        (JsPath \ "agi_vector").read[AgiVector].reads(js).asOpt.map(propList += _)

        JsSuccess(new CZMLPacket(id, name, parent, description, version, propList))
      }
    }

    val theWrites = new Writes[CZMLPacket] {
      def writes(packet: CZMLPacket) = {
        val theList = ListBuffer[Option[(String, JsValue)]](
          packet.id.map("id" -> JsString(_)),
          packet.name.map("name" -> JsString(_)),
          packet.parent.map("parent" -> JsString(_)),
          packet.description.map("description" -> JsString(_)),
          packet.version.map("version" -> JsString(_)))

        packet.propertyList.foreach({
          case x: Availability => theList += Availability.fmt.writes(x).asOpt[Availability].map("availability" -> Json.toJson(_))
          case x: CzmlPositions => theList += CzmlPositions.fmt.writes(x).asOpt[CzmlPositions].map("position" -> Json.toJson(_))
          case x: Billboard => theList += Billboard.fmt.writes(x).asOpt[Billboard].map("billboard" -> Json.toJson(_))
          case x: Orientation => theList += Orientation.fmt.writes(x).asOpt[Orientation].map("orientation" -> Json.toJson(_))
          case x: Point => theList += Point.fmt.writes(x).asOpt[Point].map("point" -> Json.toJson(_))
          case x: Label => theList += Label.fmt.writes(x).asOpt[Label].map("label" -> Json.toJson(_))
          case x: Polyline => theList += Polyline.fmt.writes(x).asOpt[Polyline].map("polyline" -> Json.toJson(_))
          case x: Path => theList += Path.fmt.writes(x).asOpt[Path].map("path" -> Json.toJson(_))
          case x: Polygon => theList += Polygon.fmt.writes(x).asOpt[Polygon].map("polygon" -> Json.toJson(_))
          case x: Ellipsoid => theList += Ellipsoid.fmt.writes(x).asOpt[Ellipsoid].map("ellipsoid" -> Json.toJson(_))
          case x: Radii => theList += Radii.fmt.writes(x).asOpt[Radii].map("viewFrom" -> Json.toJson(_))
          case x: Rectangle => theList += Rectangle.fmt.writes(x).asOpt[Rectangle].map("rectangle" -> Json.toJson(_))
          case x: Wall => theList += Wall.fmt.writes(x).asOpt[Wall].map("wall" -> Json.toJson(_))
          case x: Model => theList += Model.fmt.writes(x).asOpt[Model].map("model" -> Json.toJson(_))
          case x: Ellipse => theList += Ellipse.fmt.writes(x).asOpt[Ellipse].map("ellipse" -> Json.toJson(_))
          case x: Clock => theList += Clock.fmt.writes(x).asOpt[Clock].map("clock" -> Json.toJson(_))

          case x: ConicSensor => theList += ConicSensor.fmt.writes(x).asOpt[ConicSensor].map("agi_conicSensor" -> Json.toJson(_))
          case x: CustomPatternSensor => theList += CustomPatternSensor.fmt.writes(x).asOpt[CustomPatternSensor].map("agi_customPatternSensor" -> Json.toJson(_))
          case x: Fan => theList += Fan.fmt.writes(x).asOpt[Fan].map("agi_fan" -> Json.toJson(_))
          case x: RectangularSensor => theList += RectangularSensor.fmt.writes(x).asOpt[RectangularSensor].map("agi_rectangularSensor" -> Json.toJson(_))
          case x: AgiVector => theList += AgiVector.fmt.writes(x).asOpt[AgiVector].map("agi_vector" -> Json.toJson(_))
          case _ =>
        })

        JsObject(theList.flatten)
      }
    }

    implicit val fmt: Format[CZMLPacket] = Format(theReads, theWrites)
  }

  /**
    * a CZML document contains a single JSON array where each object-literal element in the array is a CZML packet.
    */
  final case class CZML(packets: ListBuffer[CZMLPacket]) {

    def add(packet: CZMLPacket) = packets += packet

    def remove(packet: CZMLPacket) = packets -= packet

    /**
      * returns the whole CZML document as string made of an array of streamable packets
      */
    def asStreamData(): String = {
      val sb = new mutable.StringBuilder("[ \n")
      for (packet <- packets) sb.append(packet.asEventSource() + "\n")
      sb.append(" ]")
      sb.toString()
    }

  }

  object CZML {

    def apply(doc: String): CZML = {
      Json.parse(doc).asOpt[JsArray] match {
        case Some(jsonArray) => CZML(jsonArray)
        case None => CZML() // todo log some error
      }
    }

    def apply(jsonArray: JsArray): CZML = {
      val packetList = for (pckt <- jsonArray.value) yield Json.fromJson[CZMLPacket](pckt).asOpt
      new CZML(packetList.flatten.asInstanceOf[ListBuffer[CZMLPacket]])
    }

    def apply(): CZML = new CZML(ListBuffer[CZMLPacket]())

    val theReads = new Reads[CZML] {
      def reads(js: JsValue): JsResult[CZML] = {
        JsPath.read[Array[CZMLPacket]].reads(js).asOpt match {
          case Some(list) =>
            // this is to convert the array into a ListBuffer, there must be a better way todo
            val listBuf = ListBuffer.empty ++= (for (p <- list) yield p)
            JsSuccess(new CZML(listBuf))

          case None => JsSuccess(CZML()) // todo log some error
        }
      }
    }

    val theWrites = new Writes[CZML] {
      def writes(czml: CZML) = Json.toJson(czml.packets)
    }

    implicit val fmt: Format[CZML] = Format(theReads, theWrites)

  }

}