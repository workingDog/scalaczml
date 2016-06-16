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

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.mutable.{ArrayBuffer, HashSet}
import com.kodekutters.czml.czmlCore._
import com.kodekutters.czml.czmlCustom.CustomProperty

import scala.collection.immutable.ListMap


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
  * This package implements the CZML object as a list of CZMLPacket,
  * the CZMLPacket object and all its constituent czml properties.
  */
package object czmlProperties {

  val logger = Logger(LoggerFactory.getLogger("czmlProperties"))

  /**
    * all CzmlPacket constituent properties extend this trait
    * (see also object CzmlProperty)
    */
  trait CzmlProperty

  /**
    * a CzmlProperty representing custom properties
    *
    * @param properties a map with key = the field name, and value = a CustomProperty
    */
  case class CustomProperties(properties: Map[String, CustomProperty] = ListMap.empty) extends CzmlProperty

  object CustomProperties {

    val theReads = new Reads[CustomProperties] {
      def reads(json: JsValue): JsResult[CustomProperties] =
        JsPath.read[Map[String, CustomProperty]].map(CustomProperties(_)).reads(json)
    }

    val theWrites = new Writes[CustomProperties] {
      def writes(custom: CustomProperties) = Json.toJson(custom.properties)
    }

    implicit val fmt: Format[CustomProperties] = Format(theReads, theWrites)
  }

  /**
    * represents time availability as a String or an array of strings
    *
    * @param value the availability time value(s)
    */
  case class Availability(value: Either[String, Array[String]]) extends CzmlProperty {
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
    * A box, which is a closed rectangular cuboid
    *
    * @param show           A boolean Property specifying the visibility of the box.
    * @param dimensions     The dimensions of the box
    * @param material       A Property specifying the material used to fill the box.
    * @param fill           A boolean Property specifying whether the box is filled with the provided material.
    * @param outline        A boolean Property specifying whether the box is outlined.
    * @param outlineColor   A Property specifying the Color of the outline.
    * @param outlineWidth   A numeric Property specifying the width of the outline.
    */
  case class Box(show: Option[CzmlBoolean] = None,
                 dimensions: Option[BoxDimensions] = None,
                 material: Option[Material] = None,
                 fill: Option[CzmlBoolean] = None,
                 outline: Option[CzmlBoolean] = None,
                 outlineColor: Option[ColorProperty] = None,
                 outlineWidth: Option[CzmlNumber] = None) extends CzmlProperty

  object Box {
    implicit val fmt = Json.format[Box]
  }

  /**
    * A corridor, which is a shape defined by a centerline and width that conforms to the curvature of the globe.
    * It can be placed on the surface or at altitude and can optionally be extruded into a volume
    *
    * @param show           A boolean Property specifying the visibility of the corridor.
    * @param positions      The array of positions defining the centerline of the corridor
    * @param width          The width of the corridor
    * @param material       A Property specifying the material used to fill the corridor.
    * @param height         A numeric Property specifying the height of the corridor.
    * @param extrudedHeight A numeric Property specifying the altitude of the corridor extrusion.
    * @param granularity    A numeric Property specifying the angular distance between points on the corridor.
    * @param cornerType     The corner type of the corridor
    * @param fill           A boolean Property specifying whether the corridor is filled with the provided material.
    * @param outline        A boolean Property specifying whether the corridor is outlined.
    * @param outlineColor   A Property specifying the Color of the outline.
    * @param outlineWidth   A numeric Property specifying the width of the outline.
    *
    */
  case class Corridor(show: Option[CzmlBoolean] = None,
                      positions: Option[Positions] = None,
                      width: Option[CzmlNumber] = None,
                      height: Option[CzmlNumber] = None,
                      extrudedHeight: Option[CzmlNumber] = None,
                      cornerType: Option[CornerType] = None,
                      granularity: Option[CzmlNumber] = None,
                      material: Option[Material] = None,
                      fill: Option[CzmlBoolean] = None,
                      outline: Option[CzmlBoolean] = None,
                      outlineColor: Option[ColorProperty] = None,
                      outlineWidth: Option[CzmlNumber] = None) extends CzmlProperty

  object Corridor {
    implicit val fmt = Json.format[Corridor]
  }

  /**
    * A cylinder
    *
    * @param show         A boolean Property specifying the visibility of the cylinder.
    * @param length       The length of the cylinder
    * @param material     A Property specifying the material used to fill the cylinder.
    * @param topRadius    The top radius of the cylinder
    * @param bottomRadius The bottom radius of the cylinder
    * @param fill         A boolean Property specifying whether the cylinder is filled with the provided material.
    * @param outline      A boolean Property specifying whether the cylinder is outlined.
    * @param outlineColor A Property specifying the Color of the outline.
    * @param outlineWidth A numeric Property specifying the width of the outline.
    *
    */
  case class Cylinder(show: Option[CzmlBoolean] = None,
                      length: Option[CzmlNumber] = None,
                      topRadius: Option[CzmlNumber] = None,
                      bottomRadius: Option[CzmlNumber] = None,
                      material: Option[Material] = None,
                      fill: Option[CzmlBoolean] = None,
                      outline: Option[CzmlBoolean] = None,
                      outlineColor: Option[ColorProperty] = None,
                      outlineWidth: Option[CzmlNumber] = None) extends CzmlProperty

  object Cylinder {
    implicit val fmt = Json.format[Cylinder]
  }

  /**
    * A billboard, or viewport-aligned image. The billboard is positioned in the scene by the position property.
    * A billboard is sometimes called a marker.
    *
    * @param color            This color value is multiplied with the values of the billboard's "image" to produce the final color.
    * @param eyeOffset        The eye offset of the billboard, which is the offset in eye coordinates at which
    *                         to place the billboard relative to the position property. Eye coordinates are
    *                         a left-handed coordinate system where the X-axis points toward the viewer's right,
    *                         the Y-axis points up, and the Z-axis points into the screen.
    * @param horizontalOrigin The horizontal origin of the billboard. It controls whether the billboard
    *                         image is left-, center-, or right-aligned with the position.
    * @param image            The image displayed on the billboard, expressed as a URL. For broadest client compatibility,
    *                         the URL should be accessible via Cross-Origin Resource Sharing (CORS).
    *                         The URL may also be a data URI.
    * @param pixelOffset      The offset, in viewport pixels, of the billboard origin from the position.
    *                         A pixel offset is the number of pixels up and to the right to place the billboard,
    *                         relative to the position.
    * @param scale            The scale of the billboard. The scale is multiplied with the pixel size of the billboard's image.
    *                         For example, if the scale is 2.0, the billboard will be rendered with twice the number of pixels,
    *                         in each direction, of the image.
    * @param rotation         The rotation of the billboard offset from the alignedAxes.
    * @param alignedAxis      The aligned axis is the unit vector, in world coordinates, that the billboard up vector points towards.
    *                         The default is the zero vector, which means the billboard is aligned to the screen up vector.
    * @param show             whether or not to show this property
    * @param verticalOrigin   The vertical origin of the billboard. It controls whether the billboard image
    *                         is bottom-, center-, or top-aligned with the position.
    * @param sizeInMeters     whether this billboard's size (width and height) should be measured in meters, otherwise size is measured in pixels
    */
  case class Billboard(color: Option[ColorProperty] = None, eyeOffset: Option[CzmlCartesian] = None,
                       horizontalOrigin: Option[Origin[HORIZONTAL]] = None, image: Option[CzmlUri] = None,
                       pixelOffset: Option[CzmlCartesian2] = None, scale: Option[Number] = None,
                       rotation: Option[Number] = None,
                       alignedAxis: Option[CzmlCartesian] = None,
                       show: Option[CzmlBoolean] = None,
                       verticalOrigin: Option[Origin[VERTICAL]]= None,
                       sizeInMeters: Option[CzmlBoolean] = None) extends CzmlProperty

  object Billboard {
    implicit val fmt = Json.format[Billboard]
  }

  /**
    * The orientation of the object in the world. The orientation has no direct visual representation,
    * but it is used to orient models, cones, and pyramids attached to the object.
    *
    * @param axes
    * @param unitQuaternion The orientation specified as a 4-dimensional unit magnitude quaternion, specified as [X, Y, Z, W]
    * @param reference  A reference property.
    * @param interval   an interval
    * @param timeFields the time interpolatable part of this property
    */
  case class Orientation(axes: Option[String] = None, unitQuaternion: Option[UnitQuaternionValue] = None,
                         interval: Option[String] = None,
                         reference: Option[String] = None,
                         timeFields: Option[Interpolatable] = None) extends CzmlProperty

  object Orientation {

    val theReads: Reads[Orientation] =
      ((JsPath \ "axes").readNullable[String] and
        (JsPath \ "unitQuaternion").readNullable[UnitQuaternionValue] and
        (JsPath \ "interval").readNullable[String] and
        (JsPath \ "reference").readNullable[String] and
        Interpolatable.fmt) ((ax, uni, intrv, ref, interpo) => Orientation(ax, uni, intrv, ref, Option(interpo)))

    val theWrites: Writes[Orientation] =
      ((JsPath \ "axes").writeNullable[String] and
        (JsPath \ "unitQuaternion").writeNullable[UnitQuaternionValue] and
        (JsPath \ "interval").writeNullable[String] and
        (JsPath \ "reference").writeNullable[String] and
        JsPath.writeNullable[Interpolatable]) (unlift(Orientation.unapply))

    implicit val fmt: Format[Orientation] = Format(theReads, theWrites)
  }

  /**
    * A point, or viewport-aligned circle. The point is positioned in the scene by the position property.
    *
    * @param color        of the point.
    * @param outlineColor The color of the outline of the point.
    * @param outlineWidth The width of the outline of the point.
    * @param pixelSize    The size of the point, in pixels.
    * @param show         whether or not to show this property
    */
  case class Point(color: Option[ColorProperty] = None, outlineColor: Option[ColorProperty] = None,
                   outlineWidth: Option[Number] = None, pixelSize: Option[Number] = None,
                   show: Option[CzmlBoolean] = None) extends CzmlProperty

  object Point {
    implicit val fmt = Json.format[Point]
  }

  /**
    * A string of text. The label is positioned in the scene by the position property.
    *
    * @param eyeOffset        The eye offset of the label, which is the offset in eye coordinates at which to place
    *                         the label relative to the position property. Eye coordinates are a
    *                         left-handed coordinate system where the X-axis points toward the viewer's right,
    *                         the Y-axis points up, and the Z-axis points into the screen.
    * @param fillColor        The fill color of the label.
    * @param font             The font to use for the label.
    * @param horizontalOrigin The horizontal origin of the label.
    *                         It controls whether the label is left-, center-, or right-aligned with the position.
    * @param outlineColor     The outline color of the label.
    * @param outlineWidth     The outline width of the label.
    * @param pixelOffset      The offset, in viewport pixels, of the label origin from the position. A pixel offset is
    *                         the number of pixels up and to the right to place the label, relative to the position.
    * @param scale            The scale of the label. The scale is multiplied with the pixel size of the label's text. For example,
    *                         if the scale is 2.0, the label will be rendered with twice the number of pixels,
    *                         in each direction, of the text.
    * @param show             whether or not to show this property
    * @param style            The style of the label.
    * @param text             The text displayed by the label.
    * @param verticalOrigin   The vertical origin of the label. It controls whether the label image is bottom-, center-,
    *                         or top-aligned with the position.
    */
  case class Label(eyeOffset: Option[CzmlCartesian] = None, fillColor: Option[ColorProperty] = None, font: Option[Font] = None,
                   horizontalOrigin: Option[Origin[HORIZONTAL]] = None, outlineColor: Option[ColorProperty] = None,
                   outlineWidth: Option[Number] = None, pixelOffset: Option[CzmlCartesian2] = None,
                   scale: Option[Number] = None, show: Option[CzmlBoolean] = None, style: Option[Style] = None,
                   text: Option[Text] = None, verticalOrigin: Option[Origin[VERTICAL]] = None) extends CzmlProperty

  object Label {
    implicit val fmt = Json.format[Label]
  }

  /**
    * An array of CzmlPosition. This CzmlPositions property is used only in CZMLPacket.
    *
    * @param values the array of CzmlPosition
    */
  case class CzmlPositions(values: Option[Array[CzmlPosition]]) extends CzmlProperty {
    def this(values: Array[CzmlPosition]) = this(Option(values))

    def this(position: CzmlPosition) = this(Option(Array(position)))

    def this(x: Double, y: Double, z: Double) = this(CzmlPosition(x, y, z))

    def this(referenceFrame: String, x: Double, y: Double, z: Double) = this(CzmlPosition(referenceFrame, x, y, z))

  }

  object CzmlPositions {

    def apply(values: Array[CzmlPosition]): CzmlPositions = new CzmlPositions(Option(values))

    def apply(position: CzmlPosition): CzmlPositions = new CzmlPositions(Option(Array(position)))

    def apply(x: Double, y: Double, z: Double): CzmlPositions = new CzmlPositions(x, y, z)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double): CzmlPositions = new CzmlPositions(referenceFrame, x, y, z)


    val theReads = new Reads[CzmlPositions] {
      def reads(js: JsValue): JsResult[CzmlPositions] = {
        JsPath.read[Array[CzmlPosition]].reads(js).asOpt match {
          // have a single CzmlPosition that we wrap in an option array
          case None => JsSuccess(new CzmlPositions(Option(Array(JsPath.read[CzmlPosition].reads(js).getOrElse(new CzmlPosition())))))
          // have an array of CzmlPosition
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
            else Json.toJson(list)
        }
      }
    }

    implicit val fmt: Format[CzmlPositions] = Format(theReads, theWrites)
  }

  /**
    * A path, which is a polyline defined by the motion of an object over time.
    * The possible vertices of the path are specified by the position property.
    *
    * @param show       whether or not to show this property
    * @param material   The material to use to draw the path.
    * @param width      The width of the path line.
    * @param resolution The maximum step-size, in seconds, used to sample the path. If the position property
    *                   has data points farther apart than resolution specfies, additional steps will be taken,
    *                   creating a smoother path.
    * @param leadTime   The time ahead of the animation time, in seconds, to show the path.
    * @param trailTime  The time behind the animation time, in seconds, to show the path.
    */
  case class Path(show: Option[CzmlBoolean] = None, material: Option[PolylineMaterial] = None,
                  width: Option[Number] = None, resolution: Option[Number] = None,
                  leadTime: Option[Number] = None, trailTime: Option[Number] = None) extends CzmlProperty

  object Path {
    implicit val fmt = Json.format[Path]
  }

  /**
    * A polyline, which is a line in the scene composed of multiple segments.
    *
    * @param positions     The array of positions defining the polyline as a line strip.
    * @param show          whether or not to show this property
    * @param material      The material to use to draw the polyline.
    * @param width         The width of the polyline.
    * @param granularity   The sampling distance, in radians
    * @param followSurface Whether or not the positions are connected as great arcs (the default) or as straight lines.
    */
  case class Polyline(positions: Option[Positions] = None, show: Option[CzmlBoolean] = None,
                      material: Option[PolylineMaterial] = None, width: Option[Number] = None,
                      granularity: Option[Number] = None, followSurface: Option[CzmlBoolean] = None) extends CzmlProperty

  object Polyline {
    implicit val fmt = Json.format[Polyline]
  }

  /**
    * The rectangle conforms to the curvature of the globe and can be placed on the surface or at altitude and can optionally be extruded into a volume.
    *
    * @param coordinates    the coordinate of the rectangle in WsenDegrees
    * @param show           A boolean Property specifying the visibility of the rectangle.
    * @param material       A Property specifying the material used to fill the rectangle.
    * @param height         A numeric Property specifying the altitude of the rectangle.
    * @param extrudedHeight A numeric Property specifying the altitude of the rectangle extrusion.
    * @param granularity    A numeric Property specifying the angular distance between points on the rectangle.
    * @param rotation       A numeric property specifying the rotation of the rectangle clockwise from north.
    * @param stRotation     A numeric property specifying the rotation of the rectangle texture counter-clockwise from north.
    * @param fill           A boolean Property specifying whether the rectangle is filled with the provided material.
    * @param outline        A boolean Property specifying whether the rectangle is outlined.
    * @param outlineColor   A Property specifying the Color of the outline.
    * @param outlineWidth   A numeric Property specifying the width of the outline.
    * @param closeBottom    A boolean Property specifying whether the rectangle has a bottom cover when extruded.
    * @param closeTop       A boolean Property specifying whether the rectangle has a top cover when extruded
    */
  case class Rectangle(coordinates: Option[RectangleCoordinates] = None,
                       show: Option[CzmlBoolean] = None,
                       material: Option[Material] = None,
                       height: Option[Number] = None,
                       extrudedHeight: Option[Number] = None,
                       granularity: Option[Number] = None,
                       rotation: Option[Number] = None,
                       stRotation: Option[Number] = None,
                       fill: Option[CzmlBoolean] = None,
                       outline: Option[CzmlBoolean] = None,
                       outlineColor: Option[ColorProperty] = None,
                       outlineWidth: Option[Number] = None,
                       closeBottom: Option[CzmlBoolean] = None,
                       closeTop: Option[CzmlBoolean] = None) extends CzmlProperty {

    def this(w: Double, s: Double, e: Double, n: Double) = this(Option(RectangleCoordinates(w, s, e, n)))

  }

  object Rectangle {
    implicit val fmt = Json.format[Rectangle]
  }

  /**
    * Describes a two dimensional wall defined as a line strip and optional maximum and minimum heights.
    * The wall conforms to the curvature of the globe and can be placed along the surface or at altitude.
    *
    * @param positions      A Property specifying the array of Cartesian3 positions which define the top of the wall.
    * @param show           A boolean Property specifying the visibility of the wall.
    * @param material       A Property specifying the material used to fill the wall.
    * @param minimumHeights A Property specifying an array of heights to be used for the bottom of the wall instead of the globe surface.
    * @param maximumHeights A Property specifying an array of heights to be used for the top of the wall instead of the height of each position.
    * @param granularity    A numeric Property specifying the angular distance between each latitude and longitude point.
    * @param fill           A boolean Property specifying whether the wall is filled with the provided material.
    * @param outline        A boolean Property specifying whether the wall is outlined.
    * @param outlineColor   A Property specifying the Color of the outline.
    * @param outlineWidth   A numeric Property specifying the width of the outline.
    */
  case class Wall(positions: Option[Positions] = None,
                  show: Option[CzmlBoolean] = None,
                  material: Option[Material] = None,
                  minimumHeights: Option[Array[Double]] = None,
                  maximumHeights: Option[Array[Double]] = None,
                  granularity: Option[Number] = None,
                  fill: Option[CzmlBoolean] = None,
                  outline: Option[CzmlBoolean] = None,
                  outlineColor: Option[ColorProperty] = None,
                  outlineWidth: Option[Number] = None) extends CzmlProperty

  object Wall {
    implicit val fmt = Json.format[Wall]
  }

  /**
    * A polygon, which is a closed shape on the surface of the Earth.
    *
    * @param positions         The array of positions defining a simple polygon.
    * @param show              whether or not to show this property
    * @param material          The material to use to fill the polygon.
    * @param height            The height of the polygon when perPositionHeight is false.
    * @param extrudedHeight    The extruded height of the polygon.
    * @param granularity       The sampling distance, in radians.
    * @param stRotation        The rotation of any applied texture.
    * @param fill              Whether or not the polygon is filled.
    * @param outline           Whether or not the polygon is outlined.
    * @param outlineColor      The color of the polygon outline.
    * @param perPositionHeight Whether to use the height of each position to define the polygon or a constant height above the surface.
    */
  case class Polygon(positions: Option[Positions] = None, show: Option[CzmlBoolean] = None,
                     material: Option[Material] = None, height: Option[Number] = None,
                     extrudedHeight: Option[Number] = None, granularity: Option[Number] = None,
                     stRotation: Option[Number] = None, fill: Option[CzmlBoolean] = None,
                     outline: Option[CzmlBoolean] = None, outlineColor: Option[ColorProperty] = None,
                     outlineWidth: Option[Number] = None,
                     perPositionHeight: Option[CzmlBoolean] = None) extends CzmlProperty

  object Polygon {
    implicit val fmt = Json.format[Polygon]
  }

  /**
    * Describes a 3d Cartesian property which can optionally vary over time.
    * Can represent the dimensions of the ellipsoid radii.
    * It is also used in NodeTransformation for scale and translation
    *
    * @param cartesian  The Cartesian [X, Y, Z] in meters. If the array has three elements, the cartesian is constant.
    *                   If it has four or more elements, they are time-tagged samples arranged as
    *                   [Time, X, Y, Z, Time, X, Y, Z, Time, X, Y, Z, ...], where Time is an ISO 8601 date and
    *                   time string or seconds since epoch.
    * @param interval   Time interval
    * @param reference  A reference property.
    * @param timeFields the time interpolatable part of this property
    */
  case class CzmlCartesian(cartesian: Option[Cartesian] = None,
                           interval: Option[String] = None,
                           reference: Option[String] = None,
                           timeFields: Option[Interpolatable] = None) extends CzmlProperty {

    def this(cartesian: Cartesian, interval: String) = this(Option(cartesian), Option(interval))

    def this(x: Double, y: Double, z: Double, interval: String) = this(Option(new Cartesian(x, y, z)), Option(interval))

    def this(x: Double, y: Double, z: Double) = this(Option(new Cartesian(x, y, z)))

    def this(cartesian: Cartesian) = this(Option(cartesian))

  }

  object CzmlCartesian {

    def apply(cartesian: Cartesian, interval: String): CzmlCartesian = new CzmlCartesian(cartesian, interval)

    def apply(x: Double, y: Double, z: Double, interval: String): CzmlCartesian = new CzmlCartesian(x, y, z, interval)

    def apply(x: Double, y: Double, z: Double): CzmlCartesian = new CzmlCartesian(x, y, z)

    def apply(cartesian: Cartesian): CzmlCartesian = new CzmlCartesian(cartesian)

    val theReads: Reads[CzmlCartesian] =
      ((JsPath \ "cartesian").readNullable[Cartesian] and
        (JsPath \ "interval").readNullable[String] and
        (JsPath \ "reference").readNullable[String] and
        Interpolatable.fmt) ((cart, intrv, ref, interpo) => CzmlCartesian(cart, intrv, ref, Option(interpo)))

    val theWrites: Writes[CzmlCartesian] =
      ((JsPath \ "cartesian").writeNullable[Cartesian] and
        (JsPath \ "interval").writeNullable[String] and
        (JsPath \ "reference").writeNullable[String] and
        JsPath.writeNullable[Interpolatable]) (unlift(CzmlCartesian.unapply))

    implicit val fmt: Format[CzmlCartesian] = Format(theReads, theWrites)
  }

  /**
    * A suggested camera location when viewing an object, specified as a Cartesian position in
    * the East (x), North (y), Up (z) reference frame relative to the object's position.
    *
    * @param cartesian  The Cartesian [X, Y, Z] in meters. If the array has three elements, the cartesian is constant.
    *                   If it has four or more elements, they are time-tagged samples arranged as
    *                   [Time, X, Y, Z, Time, X, Y, Z, Time, X, Y, Z, ...], where Time is an ISO 8601 date and
    *                   time string or seconds since epoch.
    * @param reference  A reference property.
    * @param timeFields the time interpolatable part of this property
    */
  case class ViewFrom(cartesian: Option[Cartesian] = None,
                      reference: Option[String] = None,
                      timeFields: Option[Interpolatable] = None) extends CzmlProperty {

    def this(cartesian: Cartesian) = this(Option(cartesian))

    def this(x: Double, y: Double, z: Double) = this(Option(new Cartesian(x, y, z)))
  }

  object ViewFrom {

    def apply(cartesian: Cartesian): CzmlCartesian = new CzmlCartesian(cartesian)

    def apply(x: Double, y: Double, z: Double): CzmlCartesian = new CzmlCartesian(x, y, z)

    val theReads: Reads[ViewFrom] =
      ((JsPath \ "cartesian").readNullable[Cartesian] and
        (JsPath \ "reference").readNullable[String] and
        Interpolatable.fmt) ((cart, ref, interpo) => ViewFrom(cart, ref, Option(interpo)))

    val theWrites: Writes[ViewFrom] =
      ((JsPath \ "cartesian").writeNullable[Cartesian] and
        (JsPath \ "reference").writeNullable[String] and
        JsPath.writeNullable[Interpolatable]) (unlift(ViewFrom.unapply))

    implicit val fmt: Format[ViewFrom] = Format(theReads, theWrites)
  }


  /**
    * An ellipsoid, which is a closed quadric surface that is a three dimensional analogue of an ellipse.
    * The ellipsoid is positioned and oriented using the position and orientation properties.
    *
    * @param show            whether or not to show this property
    * @param radii           The dimensions of the ellipsoid.
    * @param fill            Whether or not the ellipsoid is filled.
    * @param material        The material to display on the surface of the ellipsoid.
    * @param outline         Whether or not the ellipsoid is outlined.
    * @param outlineColor    The color of the ellipsoid outline.
    * @param stackPartitions The number of times to partition the ellipsoid into stacks.
    * @param slicePartitions The number of times to partition the ellipsoid into radial slices.
    * @param subdivisions    The number of points per outline line, determining the granularity of the curvature.
    */
  case class Ellipsoid(show: Option[CzmlBoolean] = None, radii: Option[CzmlCartesian] = None,
                       fill: Option[CzmlBoolean] = None, material: Option[Material] = None,
                       outline: Option[CzmlBoolean] = None, outlineColor: Option[ColorProperty] = None,
                       stackPartitions: Option[Number] = None, slicePartitions: Option[Number] = None,
                       subdivisions: Option[Number] = None) extends CzmlProperty

  object Ellipsoid {
    implicit val fmt = Json.format[Ellipsoid]
  }

  /**
    * An ellipse, which is a closed curve on the surface of the Earth.
    * The ellipse is positioned using the position property.
    *
    * @param show                  whether or not to show this property
    * @param semiMajorAxis         The length of the ellipse's semi-major axis in meters.
    * @param semiMinorAxis         The length of the ellipse's semi-minor axis in meters.
    * @param rotation              The angle from north (counter-clockwise) in radians.
    * @param material              The material to use to fill the ellipse.
    * @param height                The height of the ellipse when perPositionHeight is false.
    * @param extrudedHeight        The extruded height of the ellipse.
    * @param granularity           The sampling distance, in radians.
    * @param stRotation            The rotation of any applied texture coordinates.
    * @param fill                  Whether or not the ellipse is filled.
    * @param outline               Whether or not the ellipse is outlined.
    * @param outlineColor          The color of the ellipse outline.
    * @param numberOfVerticalLines The number of vertical lines to use when outlining an extruded ellipse.
    */
  case class Ellipse(show: Option[CzmlBoolean] = None, semiMajorAxis: Option[Number] = None, semiMinorAxis: Option[Number] = None,
                     rotation: Option[Number] = None, material: Option[Material] = None,
                     height: Option[Number] = None, extrudedHeight: Option[Number] = None,
                     granularity: Option[Number] = None, stRotation: Option[Number] = None,
                     fill: Option[CzmlBoolean] = None, outline: Option[CzmlBoolean] = None,
                     outlineColor: Option[ColorProperty] = None,
                     numberOfVerticalLines: Option[Number] = None) extends CzmlProperty

  object Ellipse {
    implicit val fmt = Json.format[Ellipse]
  }

  /**
    * A conical sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    *
    * @param show                            whether or not to show this property
    * @param innerHalfAngle                  The inner half angle of the cone.
    * @param outerHalfAngle                  The outer half angle of the cone.
    * @param minimumClockAngle               The minimum clock angle limit of the cone.
    * @param maximumClockAngle               The maximum clock angle limit of the cone.
    * @param radius                          The radial limit of the cone.
    * @param showIntersection                Whether or not the intersection of the cone with the Earth is shown.
    * @param intersectionColor               The color of the intersection of the cone with the Earth.
    * @param intersectionWidth               The width of the intersection in pixels.
    * @param showLateralSurfaces             Whether or not the intersections of the cone with the earth are shown.
    * @param lateralSurfaceMaterial          Whether or not lateral surfaces are shown.
    * @param showEllipsoidSurfaces           Whether or not ellipsoid surfaces are shown.
    * @param ellipsoidSurfaceMaterial        The material to use for the cone's ellipsoid surface.
    * @param showEllipsoidHorizonSurfaces    Whether or not ellipsoid horizon surfaces are shown.
    * @param ellipsoidHorizonSurfaceMaterial The material to use for the cone's ellipsoid horizon surface.
    * @param showDomeSurfaces                Whether or not dome surfaces are shown.
    * @param domeSurfaceMaterial             The material to use for the cone's dome.
    * @param portionToDisplay                Indicates what part of a sensor should be displayed.
    */
  case class ConicSensor(show: Option[CzmlBoolean] = None, innerHalfAngle: Option[Number] = None,
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
    *
    * @param show                            whether or not to show this property
    * @param directions                      The list of directions defining the pyramid.
    * @param radius                          The radial limit of the pyramid.
    * @param showIntersection                Whether or not the intersection of the pyramid with the Earth is shown.
    * @param intersectionColor               The color of the intersection of the pyramid with the Earth.
    * @param intersectionWidth               The width of the intersection in pixels.
    * @param showLateralSurfaces             Whether or not the intersections of the pyramid with the earth are shown.
    * @param lateralSurfaceMaterial          Whether or not lateral surfaces are shown.
    * @param showEllipsoidHorizonSurfaces    Whether or not ellipsoid surfaces are shown.
    * @param ellipsoidHorizonSurfaceMaterial The material to use for the pyramid's ellipsoid surface.
    * @param showDomeSurfaces                Whether or not ellipsoid horizon surfaces are shown.
    * @param domeSurfaceMaterial             The material to use for the pyramid's dome.
    * @param portionToDisplay                Indicates what part of a sensor should be displayed.
    */
  case class CustomPatternSensor(show: Option[CzmlBoolean] = None, directions: Option[Directions] = None,
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
    *
    * @param show               whether or not to show this property
    * @param directions         The list of directions defining the fan.
    * @param radius             The radial limit of the fan.
    * @param perDirectionRadius When true, the magnitude of each direction is used instead of a constant radius.
    * @param material           The material to display on the surface of the fan.
    * @param fill               Whether or not the fan is filled.
    * @param outline            Whether or not the fan is outlined.
    * @param numberOfRings      The number of outline rings to draw, starting from the outer edge and equidistantly spaced towards the center.
    * @param outlineColor       The color of the fan outline.
    */
  case class Fan(show: Option[CzmlBoolean] = None,
                 directions: Option[Directions] = None, radius: Option[Number] = None,
                 perDirectionRadius: Option[CzmlBoolean] = None, material: Option[Material] = None,
                 fill: Option[CzmlBoolean] = None, outline: Option[CzmlBoolean] = None,
                 numberOfRings: Option[Number] = None, outlineColor: Option[ColorProperty] = None) extends CzmlProperty

  object Fan {
    implicit val fmt = Json.format[Fan]
  }

  /**
    * A rectangular pyramid sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    *
    * @param show                            whether or not to show this property
    * @param xHalfAngle                      The X half angle.
    * @param yHalfAngle                      The Y half angle.
    * @param radius                          The radial limit of the pyramid.
    * @param showIntersection                Whether or not the intersection of the pyramid with the Earth is shown.
    * @param intersectionColor               The color of the intersection of the pyramid with the Earth.
    * @param intersectionWidth               The width of the intersection in pixels.
    * @param showLateralSurfaces             Whether or not the intersections of the pyramid with the earth are shown.
    * @param lateralSurfaceMaterial          the lateral Surface Material
    * @param showEllipsoidSurfaces           Whether or not ellipsoid surfaces are shown.
    * @param ellipsoidSurfaceMaterial        The material to use for the pyramid's ellipsoid surface.
    * @param showEllipsoidHorizonSurfaces    Whether or not ellipsoid horizon surfaces are shown.
    * @param ellipsoidHorizonSurfaceMaterial The material to use for the pyramid's ellipsoid horizon surface.
    * @param showDomeSurfaces                Whether or not dome surfaces are shown.
    * @param domeSurfaceMaterial             The material to use for the pyramid's dome.
    * @param portionToDisplay                Indicates what part of a sensor should be displayed.
    */
  case class RectangularSensor(show: Option[CzmlBoolean] = None, xHalfAngle: Option[Number] = None,
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
    *
    * @param show                  whether or not to show this property
    * @param color                 of this property
    * @param direction             The direction of the vector.
    * @param length                The graphical length of the vector.
    * @param minimumLengthInPixels The minimum graphical length of the vector in pixels.
    */
  case class AgiVector(show: Option[CzmlBoolean] = None, color: Option[ColorProperty] = None,
                       direction: Option[Directions] = None, length: Option[Number] = None,
                       minimumLengthInPixels: Option[Number] = None) extends CzmlProperty

  object AgiVector {
    implicit val fmt = Json.format[AgiVector]
  }

  /**
    * The clock settings for the entire data set. Only valid on the document object.
    *
    * @param currentTime The current time.
    * @param multiplier  The multiplier, which in TICK_DEPENDENT mode is the number of seconds to advance each tick.
    *                    In SYSTEM_CLOCK_DEPENDENT mode, it is the multiplier applied to the amount of
    *                    time elapsed between ticks. This value is ignored in SYSTEM_CLOCK mode.
    * @param range       The behavior of a clock when its current time reaches its start or end points.
    *                    Valid values are 'UNBOUNDED', 'CLAMPED', and 'LOOP_STOP'.
    * @param step        Defines how a clock steps in time. Valid values are 'SYSTEM_CLOCK',
    *                    'SYSTEM_CLOCK_MULTIPLIER', and 'TICK_DEPENDENT'.
    * @param interval    an interval of time.
    */
  case class Clock(currentTime: Option[String] = None, multiplier: Option[Double] = None,
                   range: Option[String] = None, step: Option[String] = None,
                   interval: Option[String] = None) extends CzmlProperty

  object Clock {
    implicit val fmt = Json.format[Clock]
  }

  /**
    * A 3D model. The model is positioned and oriented using the position and orientation properties.
    *
    * @param show                whether or not to show this property
    * @param scale               The scale of the property.
    * @param minimumPixelSize    The approximate minimum pixel size of the model regardless of zoom.
    * @param gltf                The URL of a glTF model.
    * @param incrementallyLoadTextures   Whether or not the model can be rendered before all textures have loaded
    * @param runAnimations       Whether or not to run animations.
    * @param nodeTransformations node transformations.
    */
  case class Model(show: Option[CzmlBoolean] = None, scale: Option[Number] = None,
                   minimumPixelSize: Option[Number] = None, gltf: Option[CzmlUri] = None,
                   incrementallyLoadTextures: Option[CzmlBoolean] = None,
                   runAnimations: Option[CzmlBoolean] = None,
                   nodeTransformations: Option[NodeTransformations] = None) extends CzmlProperty

  object Model {
    implicit val fmt = Json.format[Model]
  }

  /**
    * Characterises a packet in the CZML document.
    */
  trait Packet {
    def asEventSourceData(): String
  }

  /**
    * A CZML packet describes the graphical properties for a single
    * object in the scene, such as a single aircraft.
    *
    * @param id          The ID of the object described by this packet. IDs do not need to be GUIDs,
    *                    but they do need to uniquely identify a single object within a CZML source and
    *                    any other CZML sources loaded into the same scope. If this property is not specified,
    *                    the client will automatically generate a unique one. However, this prevents later packets
    *                    from referring to this object in order to, for example, add more data to it.
    * @param name        The name of the object. It does not have to be unique and is intended for user consumption.
    * @param parent      The ID of the parent object or folder.
    * @param description An HTML description of the object.
    * @param version     The CZML version being written. Only valid on the document object.
    * @param delete      Whether the client should delete all existing data for this object, identified by ID. If true,
    *                    all other properties in this packet will be ignored.
    * @param properties  The set of properties of this object
    */
  case class CZMLPacket(id: Option[String] = None, name: Option[String] = None, parent: Option[String] = None,
                        description: Option[Description] = None, version: Option[String] = None, delete: Option[Boolean] = None,
                        properties: HashSet[CzmlProperty] = HashSet.empty) extends Packet {

    // the typical first packet
    def this(id: String, version: String) = this(Option(id), None, None, None, Option(version), None)

    def this(id: String, name: String, parent: String, description: String, version: String, properties: HashSet[CzmlProperty]) =
      this(Option(id), Option(name), Option(parent), Option(new Description(description)), Option(version), None, properties)

    def this(id: String, name: String, properties: HashSet[CzmlProperty]) =
      this(Option(id), Option(name), None, None, None, None, properties)

    def this(id: String, name: String, description: String, properties: HashSet[CzmlProperty]) =
      this(Option(id), Option(name), None, Option(new Description(description)), None, None, properties)

    def this(id: String, properties: HashSet[CzmlProperty]) = this(Option(id), None, None, None, None, None, properties)

    def this(packet: CZMLPacket) = this(packet.id, packet.name, packet.parent, packet.description, packet.version, packet.delete, packet.properties)

    /**
      * returns an eventSource data representation of this packet
      */
    def asEventSourceData(): String = {
      val sb = new StringBuilder("data: \n")
      sb.append(Json.toJson(this) + "\n\n")
      sb.toString()
    }
  }

  object CZMLPacket {

    val theReads = new Reads[CZMLPacket] {
      def reads(js: JsValue): JsResult[CZMLPacket] = {

        val id = (JsPath \ "id").read[String].reads(js).asOpt
        val name = (JsPath \ "name").read[String].reads(js).asOpt
        val parent = (JsPath \ "parent").read[String].reads(js).asOpt
        val description = (JsPath \ "description").read[Description].reads(js).asOpt
        val version = (JsPath \ "version").read[String].reads(js).asOpt
        val delete = (JsPath \ "delete").read[Boolean].reads(js).asOpt

        val propList = new HashSet[CzmlProperty]()

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
        (JsPath \ "viewFrom").read[ViewFrom].reads(js).asOpt.map(propList += _)
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
        (JsPath \ "properties").read[CustomProperties].reads(js).asOpt.map(propList += _)

        JsSuccess(new CZMLPacket(id, name, parent, description, version, delete, propList))
      }
    }

    val theWrites = new Writes[CZMLPacket] {
      def writes(packet: CZMLPacket) = {
        val theSet = HashSet[Option[(String, JsValue)]](
          packet.id.map("id" -> JsString(_)),
          packet.name.map("name" -> JsString(_)),
          packet.parent.map("parent" -> JsString(_)),
          packet.description.map("description" -> Description.fmt.writes(_)),
          packet.version.map("version" -> JsString(_)),
          packet.delete.map("delete" -> JsBoolean(_)))

        packet.properties.foreach({
          case x: Availability => theSet += Availability.fmt.writes(x).asOpt[Availability].map("availability" -> Json.toJson(_))
          case x: CzmlPositions => theSet += CzmlPositions.fmt.writes(x).asOpt[CzmlPositions].map("position" -> Json.toJson(_))
          case x: Billboard => theSet += Billboard.fmt.writes(x).asOpt[Billboard].map("billboard" -> Json.toJson(_))
          case x: Orientation => theSet += Orientation.fmt.writes(x).asOpt[Orientation].map("orientation" -> Json.toJson(_))
          case x: Point => theSet += Point.fmt.writes(x).asOpt[Point].map("point" -> Json.toJson(_))
          case x: Label => theSet += Label.fmt.writes(x).asOpt[Label].map("label" -> Json.toJson(_))
          case x: Polyline => theSet += Polyline.fmt.writes(x).asOpt[Polyline].map("polyline" -> Json.toJson(_))
          case x: Path => theSet += Path.fmt.writes(x).asOpt[Path].map("path" -> Json.toJson(_))
          case x: Polygon => theSet += Polygon.fmt.writes(x).asOpt[Polygon].map("polygon" -> Json.toJson(_))
          case x: Ellipsoid => theSet += Ellipsoid.fmt.writes(x).asOpt[Ellipsoid].map("ellipsoid" -> Json.toJson(_))
          case x: CzmlCartesian => theSet += CzmlCartesian.fmt.writes(x).asOpt[CzmlCartesian].map("viewFrom" -> Json.toJson(_))
          case x: Rectangle => theSet += Rectangle.fmt.writes(x).asOpt[Rectangle].map("rectangle" -> Json.toJson(_))
          case x: Wall => theSet += Wall.fmt.writes(x).asOpt[Wall].map("wall" -> Json.toJson(_))
          case x: Model => theSet += Model.fmt.writes(x).asOpt[Model].map("model" -> Json.toJson(_))
          case x: Ellipse => theSet += Ellipse.fmt.writes(x).asOpt[Ellipse].map("ellipse" -> Json.toJson(_))
          case x: Clock => theSet += Clock.fmt.writes(x).asOpt[Clock].map("clock" -> Json.toJson(_))
          case x: ConicSensor => theSet += ConicSensor.fmt.writes(x).asOpt[ConicSensor].map("agi_conicSensor" -> Json.toJson(_))
          case x: CustomPatternSensor => theSet += CustomPatternSensor.fmt.writes(x).asOpt[CustomPatternSensor].map("agi_customPatternSensor" -> Json.toJson(_))
          case x: Fan => theSet += Fan.fmt.writes(x).asOpt[Fan].map("agi_fan" -> Json.toJson(_))
          case x: RectangularSensor => theSet += RectangularSensor.fmt.writes(x).asOpt[RectangularSensor].map("agi_rectangularSensor" -> Json.toJson(_))
          case x: AgiVector => theSet += AgiVector.fmt.writes(x).asOpt[AgiVector].map("agi_vector" -> Json.toJson(_))
          case x: CustomProperties => theSet += CustomProperties.fmt.writes(x).asOpt[CustomProperties].map("properties" -> Json.toJson(_))
        })

        JsObject(theSet.toList.flatten)
      }
    }

    implicit val fmt: Format[CZMLPacket] = Format(theReads, theWrites)
  }

  /**
    * a CZML document contains a single JSON array where each object-literal element in the array is a CZML packet.
    *
    * @param packets the list Packet of this document
    */
  case class CZML[T <: Packet](packets: ArrayBuffer[T]) {

    def add(packet: T) = packets += packet

    def remove(packet: T) = packets -= packet

  }

  object CZML {

    def apply[T <: Packet](doc: String)(implicit fmt: Reads[T]): CZML[T] = {
      Json.parse(doc).asOpt[JsArray] match {
        case Some(jsonArray) => CZML(jsonArray)
        case None =>
          logger.error("could not parse the CZML document")
          new CZML[T](ArrayBuffer[T]())
      }
    }

    def apply[T <: Packet](jsonArray: JsArray)(implicit fmt: Reads[T]): CZML[T] = {
      val packetList = for (pckt <- jsonArray.value) yield Json.fromJson(pckt)(fmt).asOpt
      new CZML[T](packetList.flatten.toBuffer.asInstanceOf[ArrayBuffer[T]])
    }

    def apply[T <: Packet](): CZML[T] = new CZML(ArrayBuffer[T]())

    implicit def CZMLReads[T <: Packet](implicit fmt: Reads[T]) = new Reads[CZML[T]] {
      def reads(js: JsValue): JsResult[CZML[T]] = {
        js match {
          case JsArray(list) =>
            val listBuf = ArrayBuffer.empty ++= (for (p <- list) yield Json.fromJson(p)(fmt).asOpt)
            JsSuccess(new CZML[T](listBuf.flatten))

          case _ =>
            logger.error("could not read the CZML document")
            JsSuccess(new CZML[T](ArrayBuffer[T]()))
        }
      }
    }

    implicit def CZMLWrites[T <: Packet](implicit fmt: Writes[T]) = new Writes[CZML[T]] {
      def writes(czml: CZML[T]) = Json.toJson(czml.packets)
    }

  }

  /**
    * a CzmlProperty
    */
  object CzmlProperty {

    val theReads = new Reads[CzmlProperty] {
      def reads(js: JsValue): JsResult[CzmlProperty] = {
        (JsPath \ "availability").read[Availability].reads(js) or
          (JsPath \ "position").read[CzmlPositions].reads(js) or
          (JsPath \ "billboard").read[Billboard].reads(js) or
          (JsPath \ "orientation").read[Orientation].reads(js) or
          (JsPath \ "point").read[Point].reads(js) or
          (JsPath \ "label").read[Label].reads(js) or
          (JsPath \ "path").read[Path].reads(js) or
          (JsPath \ "polyline").read[Polyline].reads(js) or
          (JsPath \ "polygon").read[Polygon].reads(js) or
          (JsPath \ "ellipsoid").read[Ellipsoid].reads(js) or
          (JsPath \ "viewFrom").read[CzmlCartesian].reads(js) or
          (JsPath \ "rectangle").read[Rectangle].reads(js) or
          (JsPath \ "wall").read[Wall].reads(js) or
          (JsPath \ "model").read[Model].reads(js) or
          (JsPath \ "ellipse").read[Ellipse].reads(js) or
          (JsPath \ "clock").read[Clock].reads(js) or
          (JsPath \ "agi_conicSensor").read[ConicSensor].reads(js) or
          (JsPath \ "agi_customPatternSensor").read[CustomPatternSensor].reads(js) or
          (JsPath \ "agi_fan").read[Fan].reads(js) or
          (JsPath \ "agi_rectangularSensor").read[RectangularSensor].reads(js) or
          (JsPath \ "agi_vector").read[AgiVector].reads(js)
        (JsPath \ "properties").read[CustomProperties].reads(js)
      }
    }

    val theWrites = Writes[CzmlProperty] {
      case x: Availability => Json.obj("availability" -> Availability.fmt.writes(x))
      case x: CzmlPositions => Json.obj("position" -> CzmlPositions.fmt.writes(x))
      case x: Billboard => Json.obj("billboard" -> Billboard.fmt.writes(x))
      case x: Orientation => Json.obj("orientation" -> Orientation.fmt.writes(x))
      case x: Point => Json.obj("point" -> Point.fmt.writes(x))
      case x: Label => Json.obj("label" -> Label.fmt.writes(x))
      case x: Polyline => Json.obj("polyline" -> Polyline.fmt.writes(x))
      case x: Path => Json.obj("path" -> Path.fmt.writes(x))
      case x: Polygon => Json.obj("polygon" -> Polygon.fmt.writes(x))
      case x: Ellipsoid => Json.obj("ellipsoid" -> Ellipsoid.fmt.writes(x))
      case x: CzmlCartesian => Json.obj("viewFrom" -> CzmlCartesian.fmt.writes(x))
      case x: Rectangle => Json.obj("rectangle" -> Rectangle.fmt.writes(x))
      case x: Wall => Json.obj("wall" -> Wall.fmt.writes(x))
      case x: Model => Json.obj("model" -> Model.fmt.writes(x))
      case x: Ellipse => Json.obj("ellipse" -> Ellipse.fmt.writes(x))
      case x: Clock => Json.obj("clock" -> Clock.fmt.writes(x))
      case x: ConicSensor => Json.obj("agi_conicSensor" -> ConicSensor.fmt.writes(x))
      case x: CustomPatternSensor => Json.obj("agi_customPatternSensor" -> CustomPatternSensor.fmt.writes(x))
      case x: Fan => Json.obj("agi_fan" -> Fan.fmt.writes(x))
      case x: RectangularSensor => Json.obj("agi_rectangularSensor" -> RectangularSensor.fmt.writes(x))
      case x: AgiVector => Json.obj("agi_vector" -> AgiVector.fmt.writes(x))
      case x: CustomProperties => Json.obj("properties" -> CustomProperties.fmt.writes(x))
    }

    implicit val fmt: Format[CzmlProperty] = Format(theReads, theWrites)
  }

}
