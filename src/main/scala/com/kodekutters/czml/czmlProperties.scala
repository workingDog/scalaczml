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

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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
  * This package implements the CZML object as a list of CZMLPacket,
  * the CZMLPacket object and all its constituent czml properties.
  */
package object czmlProperties {

  val logger = Logger(LoggerFactory.getLogger("czmlProperties"))

  /**
    * all CzmlPacket constituent properties extend this trait
    */
  trait CzmlProperty

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
    */
  case class Billboard(color: Option[ColorProperty] = None, eyeOffset: Option[EyeOffset] = None,
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

    def this(c: javafx.scene.paint.Color) =  this(CzmlColor(c))

    def this(c: java.awt.Color) =  this(CzmlColor(c))
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

    def apply(c: javafx.scene.paint.Color) = new Billboard(CzmlColor(c))

    def apply(c: java.awt.Color) = new Billboard(CzmlColor(c))

  }

  /**
    * The orientation of the object in the world. The orientation has no direct visual representation,
    * but it is used to orient models, cones, and pyramids attached to the object.
    *
    * @param axes
    * @param unitQuaternion
    * @param reference                     A reference property.
    * @param epoch                         Specifies the epoch to use for times specifies as seconds since an epoch.
    * @param nextTime                      The time of the next sample within this interval, specified as either an ISO 8601 date and time string
    *                                      or as seconds since epoch. This property is used to determine if there is a gap between
    *                                      samples specified in different packets.
    * @param previousTime                  The time of the previous sample within this interval, specified as either an
    *                                      ISO 8601 date and time string or as seconds since epoch.
    *                                      This property is used to determine if there is a gap between samples specified in different packets.
    * @param interpolationAlgorithm        specifies the algorithm to use to interpolate a value at a different time from the provided data
    * @param interpolationDegree           specifies the degree of the polynomial to use for interpolation
    * @param forwardExtrapolationType      the type of extrapolation to perform when a value is requested at a time after any available samples.
    * @param forwardExtrapolationDuration  the amount of time to extrapolate backward before the property becomes undefined.
    * @param backwardExtrapolationType     the type of extrapolation to perform when a value is requested at a time before any available samples.
    * @param backwardExtrapolationDuration the amount of time to extrapolate backward before the property becomes undefined.
    */
  case class Orientation(axes: Option[String] = None, unitQuaternion: Option[Array[Double]] = None,
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
    *
    * @param color        of the point.
    * @param outlineColor The color of the outline of the point.
    * @param outlineWidth The width of the outline of the point.
    * @param pixelSize    The size of the point, in pixels.
    * @param show         whether or not to show this property
    */
  case class Point(color: Option[ColorProperty] = None, outlineColor: Option[ColorProperty] = None,
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
  case class Label(eyeOffset: Option[EyeOffset] = None, fillColor: Option[ColorProperty] = None, font: Option[Font] = None,
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
  case class CzmlPositions(values: Option[Array[CzmlPosition]]) extends CzmlProperty {
    def this(values: Array[CzmlPosition]) = this(Option(values))

    def this(position: CzmlPosition) = this(Option(Array(position)))

    def this(referenceFrame: String, cartesian: Cartesian, interval: String) =
      this(CzmlPosition(referenceFrame, cartesian, interval))

    def this(referenceFrame: String, x: Double, y: Double, z: Double, interval: String) =
      this(CzmlPosition(referenceFrame, x, y, z, interval))

    def this(x: Double, y: Double, z: Double) = this(CzmlPosition(x, y, z))

    def this(referenceFrame: String, x: Double, y: Double, z: Double) = this(CzmlPosition(referenceFrame, x, y, z))

  }

  object CzmlPositions {

    def apply(values: Array[CzmlPosition]): CzmlPositions = new CzmlPositions(Option(values))

    def apply(position: CzmlPosition): CzmlPositions = new CzmlPositions(Option(Array(position)))

    def apply(referenceFrame: String, cartesian: Cartesian, interval: String): CzmlPositions = new CzmlPositions(referenceFrame, cartesian, interval)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double, interval: String): CzmlPositions = new CzmlPositions(referenceFrame, x, y, z, interval)

    def apply(x: Double, y: Double, z: Double): CzmlPositions = new CzmlPositions(x, y, z)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double): CzmlPositions = new CzmlPositions(referenceFrame, x, y, z)


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
  case class Path(show: Option[CzmlBoolean] = None, material: Option[LineMaterial] = None,
                  width: Option[Number] = None, resolution: Option[Number] = None,
                  leadTime: Option[Number] = None, trailTime: Option[Number] = None) extends CzmlProperty {

    def this(show: CzmlBoolean, material: LineMaterial, width: Number, resolution: Number, leadTime: Number, trailTime: Number) =
      this(Option(show), Option(material), Option(width), Option(resolution), Option(leadTime), Option(trailTime))

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
    *
    * @param positions     The array of positions defining the polyline as a line strip.
    * @param show          whether or not to show this property
    * @param material      The material to use to draw the polyline.
    * @param width         The width of the polyline.
    * @param followSurface Whether or not the positions are connected as great arcs (the default) or as straight lines.
    */
  case class Polyline(positions: Option[Positions] = None, show: Option[CzmlBoolean] = None,
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
    * The rectangle conforms to the curvature of the globe and can be placed on the surface or at altitude and can optionally be extruded into a volume.
    *
    * @param coordinates
    * @param show A boolean Property specifying the visibility of the rectangle.
    * @param material A Property specifying the material used to fill the rectangle.
    * @param height A numeric Property specifying the altitude of the rectangle.
    * @param extrudedHeight A numeric Property specifying the altitude of the rectangle extrusion.
    * @param granularity A numeric Property specifying the angular distance between points on the rectangle.
    * @param rotation A numeric property specifying the rotation of the rectangle clockwise from north.
    * @param stRotation A numeric property specifying the rotation of the rectangle texture counter-clockwise from north.
    * @param fill A boolean Property specifying whether the rectangle is filled with the provided material.
    * @param outline A boolean Property specifying whether the rectangle is outlined.
    * @param outlineColor A Property specifying the Color of the outline.
    * @param outlineWidth A numeric Property specifying the width of the outline.
    * @param closeBottom A boolean Property specifying whether the rectangle has a bottom cover when extruded.
    * @param closeTop A boolean Property specifying whether the rectangle has a top cover when extruded
    */
  case class Rectangle(coordinates: Option[WsenDegrees] = None, show: Option[CzmlBoolean] = None,
                       material: Option[Material] = None, height: Option[Number] = None,
                       extrudedHeight: Option[Number] = None, granularity: Option[Number] = None,
                       rotation: Option[Number] = None,
                       stRotation: Option[Number] = None, fill: Option[CzmlBoolean] = None,
                       outline: Option[CzmlBoolean] = None, outlineColor: Option[ColorProperty] = None,
                       outlineWidth: Option[Number] = None,
                       closeBottom: Option[CzmlBoolean] = None, closeTop: Option[CzmlBoolean] = None) extends CzmlProperty {

    def this(w: Double, s: Double, e: Double, n: Double) = this(Option(WsenDegrees(w, s, e, n)))

  }

  object Rectangle {
    implicit val fmt = Json.format[Rectangle]
  }

  /**
    * Describes a two dimensional wall defined as a line strip and optional maximum and minimum heights.
    * The wall conforms to the curvature of the globe and can be placed along the surface or at altitude.
    *
    * @param positions A Property specifying the array of Cartesian3 positions which define the top of the wall.
    * @param show A boolean Property specifying the visibility of the wall.
    * @param material A Property specifying the material used to fill the wall.
    * @param minimumHeights A Property specifying an array of heights to be used for the bottom of the wall instead of the globe surface.
    * @param maximumHeights A Property specifying an array of heights to be used for the top of the wall instead of the height of each position.
    * @param granularity A numeric Property specifying the angular distance between each latitude and longitude point.
    * @param fill A boolean Property specifying whether the wall is filled with the provided material.
    * @param outline A boolean Property specifying whether the wall is outlined.
    * @param outlineColor A Property specifying the Color of the outline.
    * @param outlineWidth A numeric Property specifying the width of the outline.
    */
  case class Wall(positions: Option[Positions] = None, show: Option[CzmlBoolean] = None,
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
    *
    * @param cartesian                     The radii as a Cartesian [X, Y, Z] in meters. If the array has three elements, the radii are constant.
    *                                      If it has four or more elements, they are time-tagged samples arranged as
    *                                      [Time, X, Y, Z, Time, X, Y, Z, Time, X, Y, Z, ...], where Time is an ISO 8601 date and
    *                                      time string or seconds since epoch.
    * @param interval                      Time interval
    * @param reference                     A reference property.
    * @param epoch                         Specifies the epoch to use for times specifies as seconds since an epoch.
    * @param nextTime                      The time of the next sample within this interval, specified as either an ISO 8601 date and
    *                                      time string or as seconds since epoch. This property is used to determine if there is a gap between samples specified in different packets.
    * @param previousTime                  The time of the previous sample within this interval, specified as either an ISO 8601 date and time string or as seconds since epoch.
    *                                      This property is used to determine if there is a gap between samples specified in different packets.
    * @param interpolationAlgorithm        specifies the algorithm to use to interpolate a value at a different time from the provided data
    * @param interpolationDegree           specifies the degree of the polynomial to use for interpolation
    * @param forwardExtrapolationType      the type of extrapolation to perform when a value is requested at a time after any available samples.
    * @param forwardExtrapolationDuration  the amount of time to extrapolate backward before the property becomes undefined.
    * @param backwardExtrapolationType     the type of extrapolation to perform when a value is requested at a time before any available samples.
    * @param backwardExtrapolationDuration the amount of time to extrapolate backward before the property becomes undefined.

    */
  case class Radii(cartesian: Option[Cartesian] = None, interval: Option[String] = None,
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
  case class Ellipsoid(show: Option[CzmlBoolean] = None, radii: Option[Radii] = None,
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
                       minimumLengthInPixels: Option[Number] = None) extends CzmlProperty {

    def this(show: CzmlBoolean, color: ColorProperty, direction: Directions, length: Number, minimumLengthInPixels: Number) =
      this(Option(show), Option(color), Option(direction), Option(length), Option(minimumLengthInPixels))
  }

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
    *
    * @param show                whether or not to show this property
    * @param scale               The scale of the property.
    * @param minimumPixelSize    The approximate minimum pixel size of the model regardless of zoom.
    * @param gltf                The URL of a glTF model.
    * @param runAnimations       Whether or not to run animations.
    * @param nodeTransformations node transformations.
    */
  case class Model(show: Option[CzmlBoolean] = None, scale: Option[Number] = None,
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
    * Characterises a packet in the CZML document.
    */
  trait Packet {
    def asEventSource(): String
  }

  /**
    * A CZML packet describes the graphical properties for a single
    * object in the scene, such as a single aircraft.
    *
    * @param id           The ID of the object described by this packet. IDs do not need to be GUIDs,
    *                     but they do need to uniquely identify a single object within a CZML source and
    *                     any other CZML sources loaded into the same scope. If this property is not specified,
    *                     the client will automatically generate a unique one. However, this prevents later packets
    *                     from referring to this object in order to, for example, add more data to it.
    * @param name         The name of the object. It does not have to be unique and is intended for user consumption.
    * @param parent       The ID of the parent object or folder.
    * @param description  An HTML description of the object.
    * @param version      The CZML version being written. Only valid on the document object.
    * @param properties The list of properties of this object
    */
  case class CZMLPacket(id: Option[String] = None, name: Option[String] = None, parent: Option[String] = None,
                        description: Option[String] = None, version: Option[String] = None,
                        properties: ListBuffer[CzmlProperty] = ListBuffer.empty) extends Packet {

    // the typical first packet
    def this(id: String, version: String) = this(Option(id), None, None, None, Option(version))

    def this(id: String, name: String, parent: String, description: String, version: String, properties: ListBuffer[CzmlProperty]) =
      this(Option(id), Option(name), Option(parent), Option(description), Option(version), properties)

    def this(id: String, name: String, properties: ListBuffer[CzmlProperty]) =
      this(Option(id), Option(name), None, None, None, properties)

    def this(id: String, name: String, description: String, properties: ListBuffer[CzmlProperty]) =
      this(Option(id), Option(name), None, Option(description), None, properties)

    def this(id: String, properties: ListBuffer[CzmlProperty]) = this(Option(id), None, None, None, None, properties)

    /**
      * returns an eventSource representation of this packet
      */
    def asEventSource(): String = {
      val sb = new mutable.StringBuilder("event: czml \n data: ")
      sb.append(Json.prettyPrint(Json.toJson(this)) + "\n\n")
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

        packet.properties.foreach({
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
    *
    * @param packets the list Packet of this document
    */
  case class CZML[T <: Packet](packets: ArrayBuffer[T]) {

    def add(packet: T) = packets += packet

    def remove(packet: T) = packets -= packet

    /**
      * returns the whole CZML document as a string consisting of an array of eventSource elements.
      */
    def asStreamData(): String = {
      val sb = new mutable.StringBuilder("[ \n")
      for (packet <- packets) sb.append(packet.asEventSource())
      sb.append(" ]")
      sb.toString()
    }
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

}
