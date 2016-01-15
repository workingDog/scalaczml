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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.runtime.universe._

import com.kodekutters.czml.czmlProperties._


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
  * This package implements all core czml supporting elements but not the czmlProperties.
  */
package object czmlCore {

  val logger = Logger(LoggerFactory.getLogger("czmlCore"))

  object TimeInterval {

    val fmter = DateTimeFormatter.ISO_DATE_TIME
  }

  /**
    * a time interval representation consisting of a start and stop components.
    *
    * With its implicit conversion, you can use TimeInterval methods on appropriate Strings, e.g.
    *
    * "2012-08-04T16:00:00Z/2012-08-04T18:00:00Z".start will return "2012-08-04T16:00:00Z"
    *
    * "2012-08-04T16:00:00Z/2012-08-04T18:00:00Z".startLocalDateTime will return 2012-08-04T16:00
    *
    * @param value the full time interval as a string
    */
  case class TimeInterval(value: String) {

    def this(start: String, stop: String) = this(start + "/" + stop)

    def start() = value.trim.split("/").head

    def stop() = value.trim.split("/").last

    def startLocalDateTime() = LocalDateTime.parse(start(), TimeInterval.fmter)

    def stopLocalDateTime() = LocalDateTime.parse(stop(), TimeInterval.fmter)
  }

  //    case class Duration(days: Int, seconds: Double) {
  //      def this(days: Int, hours: Int, minutes: Int, seconds: Double) = this(days, Duration.toSeconds(hours, minutes, seconds))
  //    }
  //    object Duration {
  //      def toSeconds(hours: Int, minutes: Int, seconds: Double): Double = hours * 3600.0 + minutes * 60.0 + seconds
  //      implicit val fmt = Json.format[Duration]
  //      implicit def StringToDuration(value: String): Duration = {
  //
  //      }
  //    }

  //  case class Description(reference: Option[String] = None, string: Option[String] = None)
  //  object Description {
  //    implicit val fmt = Json.format[Description]
  //  }

  /**
    * representing time as a String or a Double
    *
    * @param value the time value
    */
  case class TimeValue(value: Either[String, Double]) {
    def this(value: String) = this(Left(value))

    def this(value: Double) = this(Right(value))
  }

  object TimeValue {

    def apply(value: String): TimeValue = new TimeValue(value)

    def apply(value: Double): TimeValue = new TimeValue(value)

    val theReads = new Reads[TimeValue] {
      def reads(json: JsValue): JsResult[TimeValue] = {
        (JsPath.read[String].map(TimeValue(_)) | JsPath.read[Double].map(TimeValue(_))).reads(json)
      }
    }

    val theWrites = new Writes[TimeValue] {
      def writes(time: TimeValue) = {
        time.value match {
          case Left(x) => JsString(x)
          case Right(x) => JsNumber(x)
        }
      }
    }

    implicit val fmt: Format[TimeValue] = Format(theReads, theWrites)
  }

  /**
    * The base for a property whose value may be determined by
    * interpolating over the provided time-tagged samples.
    *
    * @param epoch                         Specifies the epoch to use for times specifies as seconds since an epoch.
    * @param nextTime                      The time of the next sample within this interval, specified as either
     an ISO 8601 date and time string or as seconds since epoch.
     This property is used to determine if there is a gap between samples specified in different packets.
    * @param previousTime                  The time of the previous sample within this interval, specified as either
     an ISO 8601 date and time string or as seconds since epoch.
     This property is used to determine if there is a gap between samples specified in different packets.
    * @param interpolationAlgorithm        specifies the algorithm to use to interpolate a value at a different time from the provided data
    * @param interpolationDegree           specifies the degree of the polynomial to use for interpolation
    * @param forwardExtrapolationType      the type of extrapolation to perform when a value is requested at a time after any available samples.
    * @param forwardExtrapolationDuration  the amount of time to extrapolate backward before the property becomes undefined.
     A value of 0 will extrapolate forever.
    * @param backwardExtrapolationType     the type of extrapolation to perform when a value is requested at a time before any available samples.
    * @param backwardExtrapolationDuration the amount of time to extrapolate backward before the property becomes undefined.
     A value of 0 will extrapolate forever.
    */
  case class Interpolatable(epoch: Option[String] = None,
                            nextTime: Option[TimeValue] = None,
                            previousTime: Option[TimeValue] = None,
                            interpolationAlgorithm: Option[String] = None,
                            interpolationDegree: Option[Int] = None,
                            forwardExtrapolationType: Option[String] = None,
                            forwardExtrapolationDuration: Option[Double] = None,
                            backwardExtrapolationType: Option[String] = None,
                            backwardExtrapolationDuration: Option[Double] = None) {

    def this(epoch: String) = this(Option(epoch))

    def this(epoch: String, interpolationAlgorithm: String, interpolationDegree: Int) =
      this(Option(epoch), None, None, Option(interpolationAlgorithm), Option(interpolationDegree))
  }

  object Interpolatable {

    implicit val fmt = Json.format[Interpolatable]

    def apply(epoch: String): Interpolatable = new Interpolatable(epoch)

    def apply(epoch: String, interpolationAlgorithm: String, interpolationDegree: Int): Interpolatable =
      new Interpolatable(epoch, interpolationAlgorithm, interpolationDegree)
  }

  /**
    * an array of west south, east north degrees coordinates for a rectangle
    */
  case class WsenDegrees(wsenDegrees: Array[Double]) {
    def this(w: Double, s: Double, e: Double, n: Double) = this(Array(w, s, e, n))
  }

  object WsenDegrees {
    implicit val fmt = Json.format[WsenDegrees]

    def apply(w: Double, s: Double, e: Double, n: Double): WsenDegrees = new WsenDegrees(w, s, e, n)
  }

  /**
    * a 3d cartesian coordinate that can have an optional time component.
    */
  case class Coordinate(t: Option[TimeValue] = None, x: Double, y: Double, z: Double) {

    def this(x: Double, y: Double, z: Double) = this(None, x, y, z)

    def this(t: String, x: Double, y: Double, z: Double) = this(Option(TimeValue(t)), x, y, z)

    def this(t: Double, x: Double, y: Double, z: Double) = this(Option(TimeValue(t)), x, y, z)

    def this(t: TimeValue, x: Double, y: Double, z: Double) = this(Option(t), x, y, z)

    def +(c: Coordinate) = new Coordinate(this.x + c.x, this.y + c.y, this.z + c.z)

    def -(c: Coordinate) = new Coordinate(this.x - c.x, this.y - c.y, this.z - c.z)

    def /(s: Double) = new Coordinate(this.x / s, this.y / s, this.z / s)

    def *(s: Double) = new Coordinate(this.x * s, this.y * s, this.z * s)

    def cross(other: Coordinate) = new Coordinate(this.y * other.z - this.z * other.y, this.z * other.x - this.x * other.z, this.x * other.y - this.y * other.x)

    def dot(other: Coordinate): Double = this.x * other.x + this.y * other.y + this.z * other.z

    def invert() = new Coordinate(-this.x, -this.y, -this.z)
  }

  object Coordinate {

    def apply(x: Double, y: Double, z: Double): Coordinate = new Coordinate(x, y, z)

    def apply(t: String, x: Double, y: Double, z: Double): Coordinate = new Coordinate(t, x, y, z)

    def apply(t: Double, x: Double, y: Double, z: Double): Coordinate = new Coordinate(t, x, y, z)

    def apply(t: TimeValue, x: Double, y: Double, z: Double): Coordinate = new Coordinate(t, x, y, z)
  }

  /**
    * a 2d cartesian coordinate that can have an optional time component.
    */
  case class Coordinate2D(t: Option[TimeValue] = None, x: Double, y: Double) {

    def this(x: Double, y: Double) = this(None, x, y)

    def this(x: Int, y: Int) = this(None, x.toDouble, y.toDouble)

    def this(t: String, x: Double, y: Double) = this(Option(TimeValue(t)), x, y)

    def this(t: Double, x: Double, y: Double) = this(Option(TimeValue(t)), x, y)

    def this(t: TimeValue, x: Double, y: Double) = this(Option(t), x, y)

    def +(c: Coordinate2D) = new Coordinate2D(this.x + c.x, this.y + c.y)

    def -(c: Coordinate2D) = new Coordinate2D(this.x - c.x, this.y - c.y)

    def /(s: Double) = new Coordinate2D(this.x / s, this.y / s)

    def *(s: Double) = new Coordinate2D(this.x * s, this.y * s)

    def dot(other: Coordinate2D) = this.x * other.x + this.y * other.y

    def invert() = new Coordinate2D(-this.x, -this.y)
  }

  object Coordinate2D {
    def apply(x: Double, y: Double): Coordinate2D = new Coordinate2D(x, y)

    def apply(x: Int, y: Int): Coordinate2D = new Coordinate2D(x.toDouble, y.toDouble)

    def apply(t: String, x: Double, y: Double): Coordinate2D = new Coordinate2D(t, x, y)

    def apply(t: Double, x: Double, y: Double): Coordinate2D = new Coordinate2D(t, x, y)

    def apply(t: TimeValue, x: Double, y: Double): Coordinate2D = new Coordinate2D(t, x, y)

  }

  /**
    * A sequence of three dimensional Cartesian coordinates specified as [X, Y, Z]. If the array has three elements,
    * the position is constant. If it has four or more elements, they are time-tagged samples
    * arranged as [Time, X, Y, Z, Time, X, Y, Z, Time, X, Y, Z, ...], where Time is an ISO 8601 date
    * and time string or seconds since epoch.
    */
  case class Cartesian(coordinates: Seq[Coordinate]) {
    def this(coordinate: Coordinate) = this(Seq(coordinate))

    def this(x: Double, y: Double, z: Double) = this(Seq(Coordinate(x, y, z)))

    def this(t: String, x: Double, y: Double, z: Double) = this(Seq(Coordinate(t, x, y, z)))

    def this(t: Double, x: Double, y: Double, z: Double) = this(Seq(Coordinate(t, x, y, z)))

    def this(t: TimeValue, x: Double, y: Double, z: Double) = this(Seq(Coordinate(t, x, y, z)))

  }

  object Cartesian {

    def ZERO(): Cartesian = new Cartesian(0.0, 0.0, 0.0)

    def apply(x: Double, y: Double, z: Double): Cartesian = new Cartesian(x, y, z)

    def apply(coordinate: Coordinate): Cartesian = new Cartesian(coordinate)

    def apply(t: String, x: Double, y: Double, z: Double): Cartesian = new Cartesian(Coordinate(t, x, y, z))

    def apply(t: Double, x: Double, y: Double, z: Double): Cartesian = new Cartesian(Coordinate(t, x, y, z))

    def apply(t: TimeValue, x: Double, y: Double, z: Double): Cartesian = new Cartesian(Coordinate(t, x, y, z))

    val theReads = new Reads[Cartesian] {
      def reads(js: JsValue): JsResult[Cartesian] = {
        val jsList = js.as[JsArray].value
        // have a list of timed coordinates, multiple of 4 elements
        if (jsList.length >= 4 && (jsList.length % 4) == 0) {
          JsSuccess(new Cartesian(
            (for (i <- jsList.indices by 4) yield ((JsPath \ i).readNullable[TimeValue] and
              (JsPath \ (i + 1)).read[Double] and (JsPath \ (i + 2)).read[Double] and
              (JsPath \ (i + 3)).read[Double]) (Coordinate.apply(_, _, _, _)).reads(js).asOpt).flatten))
        }
        // have a single coordinate [x,y,z]
        else {
          val result = ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double] and
            (JsPath \ 2).read[Double]) (Coordinate.apply(None, _, _, _)).reads(js)
          result match {
            case s: JsSuccess[Coordinate] => JsSuccess(new Cartesian(Seq(s.get)))
            case e: JsError =>
              logger.error("could not read Coordinate values: " + js.toString)
              JsError("could not read Coordinate values: " + js.toString)
          }
        }
      }
    }

    val theWrites = new Writes[Cartesian] {
      def writes(cart: Cartesian) = {
        val coordList = for (coord <- cart.coordinates) yield {
          coord.t match {
            case Some(time) => List(TimeValue.fmt.writes(time), JsNumber(coord.x), JsNumber(coord.y), JsNumber(coord.z))
            case None => List(JsNumber(coord.x), JsNumber(coord.y), JsNumber(coord.z))
          }
        }
        JsArray(coordList.flatten)
      }
    }

    implicit val fmt: Format[Cartesian] = Format(theReads, theWrites)
  }

  /**
    * A list of 2D Cartesian [X, Y] in viewport coordinates in pixels, where X is pixels to the right and Y is pixels up.
    * If the array has two elements, the pixel offset is constant.
    * If it has three or more elements, they are time-tagged samples arranged as
    * [Time, X, Y, Time, X, Y, Time, X, Y, ...], where _Time_ is an ISO 8601 date and time string
    * or seconds since epoch.
    */
  case class Cartesian2D(coordinates: Seq[Coordinate2D]) {
    def this(coordinate2D: Coordinate2D) = this(Seq(coordinate2D))

    def this(x: Double, y: Double) = this(Coordinate2D(x, y))

    def this(t: TimeValue, x: Double, y: Double) = this(Coordinate2D(t, x, y))

    def this(t: String, x: Double, y: Double) = this(Seq(Coordinate2D(t, x, y)))

    def this(t: Double, x: Double, y: Double) = this(Seq(Coordinate2D(t, x, y)))

    def this(x: Int, y: Int) = this(Coordinate2D(x, y))

  }

  object Cartesian2D {

    def ZERO(): Cartesian2D = new Cartesian2D(0.0, 0.0)

    def apply(t: TimeValue, x: Double, y: Double): Cartesian2D = new Cartesian2D(t, x, y)

    def apply(t: String, x: Double, y: Double): Cartesian2D = new Cartesian2D(t, x, y)

    def apply(t: Double, x: Double, y: Double): Cartesian2D = new Cartesian2D(t, x, y)

    def apply(x: Double, y: Double): Cartesian2D = new Cartesian2D(x, y)

    def apply(x: Int, y: Int): Cartesian2D = new Cartesian2D(x, y)

    def apply(coordinate: Coordinate2D): Cartesian2D = new Cartesian2D(coordinate)

    val theReads = new Reads[Cartesian2D] {
      def reads(js: JsValue): JsResult[Cartesian2D] = {
        val jsList = js.as[JsArray].value
        // have a list of timed coordinates, multiple of 3 elements
        if (jsList.length >= 3 && (jsList.length % 3) == 0) {
          JsSuccess(new Cartesian2D((
            for (i <- jsList.indices by 3) yield
              ((JsPath \ i).readNullable[TimeValue] and
                (JsPath \ (i + 1)).read[Double] and
                (JsPath \ (i + 2)).read[Double]) (Coordinate2D.apply(_, _, _)).reads(js).asOpt).flatten))
        }
        // have a single coordinate2D [x,y]
        else {
          val result = ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double]) (Coordinate2D.apply(None, _, _)).reads(js)
          result match {
            case s: JsSuccess[Coordinate2D] => JsSuccess(new Cartesian2D(Seq(s.get)))
            case e: JsError =>
              logger.error("could not read Coordinate2D values: " + js.toString)
              JsError("could not read Coordinate2D values: " + js.toString)
          }
        }
      }
    }

    val theWrites = new Writes[Cartesian2D] {
      def writes(cart: Cartesian2D) = {
        val coordList = for (coord <- cart.coordinates) yield {
          coord.t match {
            case Some(time) => List(TimeValue.fmt.writes(time), JsNumber(coord.x), JsNumber(coord.y))
            case None => List(JsNumber(coord.x), JsNumber(coord.y))
          }
        }
        JsArray(coordList.flatten)
      }
    }

    implicit val fmt: Format[Cartesian2D] = Format(theReads, theWrites)
  }

  /**
    * represents the geographic coordinate radian "type"
    */
  sealed trait RADIAN

  /**
    * represents the geographic coordinate degree "type"
    */
  sealed trait DEGREE

  /**
    * A timed geographic coordinate (time,long,lat,alt) coordinate.
    * The optional time value is either a String or a Double wrapped in a TimeValue object.
    * The geographic coordinate values can represent either
    * degrees or radians depending on the type parameter [T], either DEGREE or RADIAN.
    */
  case class LngLatAlt[T: TypeTag](t: Option[TimeValue] = None, lng: Double, lat: Double, alt: Double) {

    def this(lng: Double, lat: Double, alt: Double) = this(None, lng, lat, alt)

    def this(lng: Double, lat: Double) = this(None, lng, lat, 0.0)

    def this(t: TimeValue, lng: Double, lat: Double, alt: Double) = this(Option(t), lng, lat, alt)

    def this(t: String, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)

    def this(t: Double, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)

    def isRadian() = typeOf[T] =:= typeOf[RADIAN]

    def isDegree() = typeOf[T] =:= typeOf[DEGREE]

  }

  object LngLatAlt {
    def apply[T: TypeTag](lng: Double, lat: Double, alt: Double): LngLatAlt[T] = new LngLatAlt[T](lng, lat, alt)

    def apply[T: TypeTag](lng: Double, lat: Double): LngLatAlt[T] = new LngLatAlt[T](lng, lat)

    def apply[T: TypeTag](t: TimeValue, lng: Double, lat: Double, alt: Double): LngLatAlt[T] = new LngLatAlt[T](Option(t), lng, lat, alt)

    def apply[T: TypeTag](t: String, lng: Double, lat: Double, alt: Double): LngLatAlt[T] = new LngLatAlt[T](TimeValue(t), lng, lat, alt)

    def apply[T: TypeTag](t: Double, lng: Double, lat: Double, alt: Double): LngLatAlt[T] = new LngLatAlt[T](TimeValue(t), lng, lat, alt)
  }

  /**
    * A list of geodetic, WGS84 positions using longitude, latitude, and height components.
    * The positions represented as a WGS 84 Cartographic [Longitude, Latitude, Height]
    * where longitude and latitude are in degrees or radians depending on the type parameter [T] (DEGREE or RADIAN),
    * and height is in meters.
    * If the array has three elements, the position is constant.
    * If it has four or more elements, they are time-tagged samples arranged
    * as [Time, Longitude, Latitude, Height, Time, Longitude, Latitude, Height, ...],
    * where Time is an ISO 8601 date and time string or seconds since "epoch".
    *
    */
  case class Cartographic[T: TypeTag](coordinates: Seq[LngLatAlt[T]]) {

    def this(lngLatAltT: LngLatAlt[T]) = this(Seq(lngLatAltT))

    def this(lng: Double, lat: Double, alt: Double) = this(LngLatAlt[T](lng, lat, alt))

    def this(lng: Double, lat: Double) = this(LngLatAlt[T](lng, lat))

    def this(t: TimeValue, lng: Double, lat: Double, alt: Double) = this(LngLatAlt[T](t, lng, lat, alt))

    def this(t: String, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)

    def this(t: Double, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)

    def isRadian() = typeOf[T] =:= typeOf[RADIAN]

    def isDegree() = typeOf[T] =:= typeOf[DEGREE]
  }

  object Cartographic {

    def apply[T: TypeTag](lngLatAltT: LngLatAlt[T]): Cartographic[T] = new Cartographic[T](Seq(lngLatAltT))

    def apply[T: TypeTag](lng: Double, lat: Double, alt: Double): Cartographic[T] = new Cartographic[T](LngLatAlt[T](lng, lat, alt))

    def apply[T: TypeTag](lng: Double, lat: Double): Cartographic[T] = new Cartographic[T](LngLatAlt[T](lng, lat))

    def apply[T: TypeTag](t: TimeValue, lng: Double, lat: Double, alt: Double): Cartographic[T] = new Cartographic[T](LngLatAlt[T](t, lng, lat, alt))

    def apply[T: TypeTag](t: String, lng: Double, lat: Double, alt: Double): Cartographic[T] = new Cartographic[T](TimeValue(t), lng, lat, alt)

    def apply[T: TypeTag](t: Double, lng: Double, lat: Double, alt: Double): Cartographic[T] = new Cartographic[T](TimeValue(t), lng, lat, alt)

    implicit def theReads[T: TypeTag] = new Reads[Cartographic[T]] {
      def reads(js: JsValue): JsResult[Cartographic[T]] = {
        val jsList = js.as[JsArray].value
        // have a list of timed coordinates, multiple of 4 elements
        if (jsList.length >= 4 && (jsList.length % 4) == 0) {
          JsSuccess(new Cartographic(
            (for (i <- jsList.indices by 4) yield
              ((JsPath \ i).readNullable[TimeValue] and
                (JsPath \ (i + 1)).read[Double] and (JsPath \ (i + 2)).read[Double] and
                (JsPath \ (i + 3)).read[Double]) (LngLatAlt.apply[T](_, _, _, _)).reads(js).asOpt).flatten))
        }
        // have a single coordinate [lng,lat,alt]
        else {
          val result = ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double] and
            (JsPath \ 2).read[Double]) (LngLatAlt.apply(None, _, _, _)).reads(js)
          result match {
            case s: JsSuccess[LngLatAlt[T]] => JsSuccess(new Cartographic[T](Seq(s.get)))
            case e: JsError =>
              logger.error("could not read LngLatAlt values: " + js.toString)
              JsError("could not read LngLatAlt values: " + js.toString)
          }
        }
      }
    }

    implicit def theWrites[T: TypeTag] = new Writes[Cartographic[T]] {
      def writes(cart: Cartographic[T]) = {
        val coordList = for (coord <- cart.coordinates) yield {
          coord.t match {
            case Some(time) => List(TimeValue.fmt.writes(time), JsNumber(coord.lng), JsNumber(coord.lat), JsNumber(coord.alt))
            case None => List(JsNumber(coord.lng), JsNumber(coord.lat), JsNumber(coord.alt))
          }
        }
        JsArray(coordList.flatten)
      }
    }

  }

  /**
    * A timed velocity element.
    */
  case class Velocity(t: Option[TimeValue] = None, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) {
    def this(x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) = this(None, x, y, z, vx, vy, vz)

    def this(t: TimeValue, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) =
      this(Option(t), x, y, z, vx, vy, vz)

    def this(t: String, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) =
      this(TimeValue(t), x, y, z, vx, vy, vz)

    def this(t: Double, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) =
      this(TimeValue(t), x, y, z, vx, vy, vz)

  }

  object Velocity {
    def apply(x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): Velocity =
      new Velocity(None, x, y, z, vx, vy, vz)

    def apply(t: TimeValue, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): Velocity =
      new Velocity(Option(t), x, y, z, vx, vy, vz)

    def apply(t: String, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): Velocity =
      new Velocity(TimeValue(t), x, y, z, vx, vy, vz)

    def apply(t: Double, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): Velocity =
      new Velocity(TimeValue(t), x, y, z, vx, vy, vz)

  }

  /**
    * A list of Velocities consisting of three dimensional Cartesian coordinates and their derivatives specified as [X, Y, Z, vX, vY, vZ].
    * If the array has six elements, the position is constant. If it has seven or more elements,
    * they are time-tagged samples arranged as
    * [Time, X, Y, Z, vX, vY, vZ, Time, X, Y, Z, vX, vY, vZ, Time, X, Y, Z, vX, vY, vZ, ...],
    * where Time is an ISO 8601 date and time string or seconds since epoch.
    */
  case class CartesianVelocity(velocities: Seq[Velocity]) {
    def this(velocity: Velocity) = this(Seq(velocity))

    def this(x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) =
      this(new Velocity(None, x, y, z, vx, vy, vz))

    def this(t: TimeValue, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) =
      this(new Velocity(t, x, y, z, vx, vy, vz))

    def this(t: String, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) =
      this(TimeValue(t), x, y, z, vx, vy, vz)

    def this(t: Double, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) =
      this(TimeValue(t), x, y, z, vx, vy, vz)

  }

  object CartesianVelocity {

    def apply(x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): CartesianVelocity =
      new CartesianVelocity(x, y, z, vx, vy, vz)

    def apply(t: TimeValue, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): CartesianVelocity =
      new CartesianVelocity(t, x, y, z, vx, vy, vz)

    def apply(t: String, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): CartesianVelocity =
      new CartesianVelocity(TimeValue(t), x, y, z, vx, vy, vz)

    def apply(t: Double, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double): CartesianVelocity =
      new CartesianVelocity(TimeValue(t), x, y, z, vx, vy, vz)


    val theReads = new Reads[CartesianVelocity] {
      def reads(js: JsValue): JsResult[CartesianVelocity] = {
        val jsList = js.as[JsArray].value
        // have a list of timed velocities, multiple of 7 elements
        if (jsList.length >= 7 && (jsList.length % 7) == 0) {
          JsSuccess(new CartesianVelocity(
            (for (i <- jsList.indices by 7) yield ((JsPath \ i).readNullable[TimeValue] and
              (JsPath \ (i + 1)).read[Double] and (JsPath \ (i + 2)).read[Double] and
              (JsPath \ (i + 3)).read[Double] and (JsPath \ (i + 4)).read[Double] and
              (JsPath \ (i + 5)).read[Double] and (JsPath \ (i + 6)).read[Double]
              ) (Velocity.apply(_, _, _, _, _, _, _)).reads(js).asOpt).flatten))
        }
        // have a single velocity [X, Y, Z, vX, vY, vZ]
        else {
          val result = ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double] and
            (JsPath \ 2).read[Double] and (JsPath \ 3).read[Double] and
            (JsPath \ 4).read[Double] and (JsPath \ 5).read[Double]
            ) (Velocity.apply(None, _, _, _, _, _, _)).reads(js)
          result match {
            case s: JsSuccess[Velocity] => JsSuccess(new CartesianVelocity(Seq(s.get)))
            case e: JsError =>
              logger.error("could not read Velocity values: " + js.toString)
              JsError("could not read Velocity values: " + js.toString)
          }
        }
      }
    }

    val theWrites = new Writes[CartesianVelocity] {
      def writes(cart: CartesianVelocity) = {
        val coordList = for (coord <- cart.velocities) yield {
          coord.t match {
            case Some(time) => List(TimeValue.fmt.writes(time), JsNumber(coord.x), JsNumber(coord.y), JsNumber(coord.z), JsNumber(coord.vx), JsNumber(coord.vy), JsNumber(coord.vz))
            case None => List(JsNumber(coord.x), JsNumber(coord.y), JsNumber(coord.z), JsNumber(coord.vx), JsNumber(coord.vy), JsNumber(coord.vz))
          }
        }
        JsArray(coordList.flatten)
      }
    }

    implicit val fmt: Format[CartesianVelocity] = Format(theReads, theWrites)
  }

  /**
    * describes a boolean interval, i.e. a time interval and an associated boolean value
    */
  case class BooleanInterval(interval: Option[String] = None, boolean: Option[Boolean]) {
    def this(interval: String, boolean: Boolean) = this(Option(interval), Option(boolean))
  }

  object BooleanInterval {
    implicit val fmt = Json.format[BooleanInterval]

    def apply(interval: String, boolean: Boolean): BooleanInterval = new BooleanInterval(interval, boolean)
  }

  /**
    * a generic boolean property that can be a simple boolean or an array of boolean intervals
    *
    * @param value could be a simple boolean or an array of BooleanInterval
    */
  case class CzmlBoolean(value: Either[Boolean, Array[BooleanInterval]]) {
    def this(value: Boolean) = this(Left(value))

    def this(value: Array[BooleanInterval]) = this(Right(value))

    def this(value: BooleanInterval) = this(Right(Array(value)))

    def this(interval: String, boolean: Boolean) = this(Right(Array(new BooleanInterval(interval, boolean))))
  }

  object CzmlBoolean {

    def apply(value: Boolean): CzmlBoolean = new CzmlBoolean(value)

    def apply(interval: String, boolean: Boolean): CzmlBoolean = new CzmlBoolean(interval, boolean)

    val theReads = new Reads[CzmlBoolean] {
      def reads(json: JsValue): JsResult[CzmlBoolean] = {
        (JsPath.read[Boolean].map(CzmlBoolean(_)) |
          JsPath.read[Array[BooleanInterval]].map(new CzmlBoolean(_))).reads(json)
      }
    }

    val theWrites = new Writes[CzmlBoolean] {
      def writes(shw: CzmlBoolean) = {
        shw.value match {
          case Left(x) => JsBoolean(x)
          case Right(x) => Json.toJson(x)
        }
      }
    }

    implicit val fmt: Format[CzmlBoolean] = Format(theReads, theWrites)
  }

  /**
    * A color specified as an array of color components [Red, Green, Blue, Alpha]
    * where each component is in the range 0-255.
    */
  case class Rgba(t: Option[TimeValue] = None, r: Int, g: Int, b: Int, a: Int) {
    def this(t: TimeValue, r: Int, g: Int, b: Int, a: Int) = this(Option(t), r, g, b, a)

    def this(r: Int, g: Int, b: Int, a: Int) = this(None, r, g, b, a)

    def this(t: String, r: Int, g: Int, b: Int, a: Int) = this(TimeValue(t), r, g, b, a)

    def this(t: Double, r: Int, g: Int, b: Int, a: Int) = this(TimeValue(t), r, g, b, a)

    def this(c: java.awt.Color) = this(c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def this(t: String, c: java.awt.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def this(t: Double, c: java.awt.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)
  }

  object Rgba {
    implicit val fmt = Json.format[Rgba]

    def apply(t: TimeValue, r: Int, g: Int, b: Int, a: Int): Rgba = new Rgba(Option(t), r, g, b, a)

    def apply(t: String, r: Int, g: Int, b: Int, a: Int): Rgba = new Rgba(TimeValue(t), r, g, b, a)

    def apply(t: Double, r: Int, g: Int, b: Int, a: Int): Rgba = new Rgba(TimeValue(t), r, g, b, a)

    def apply(r: Int, g: Int, b: Int, a: Int): Rgba = new Rgba(None, r, g, b, a)

    def apply(c: java.awt.Color): Rgba = new Rgba(c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def apply(t: String, c: java.awt.Color): Rgba = new Rgba(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def apply(t: Double, c: java.awt.Color): Rgba = new Rgba(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)
  }

  /**
    * A list of Rgba colors.
    */
  case class RgbaList(values: Seq[Rgba]) {
    def this(rgba: Rgba) = this(Seq(rgba))

    def this(r: Int, g: Int, b: Int, a: Int) = this(Rgba(r, g, b, a))

    def this(t: String, r: Int, g: Int, b: Int, a: Int) = this(Rgba(TimeValue(t), r, g, b, a))

    def this(t: Double, r: Int, g: Int, b: Int, a: Int) = this(Rgba(TimeValue(t), r, g, b, a))

    def this(t: TimeValue, r: Int, g: Int, b: Int, a: Int) = this(Rgba(t, r, g, b, a))

    def this(c: java.awt.Color) = this(c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def this(t: String, c: java.awt.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def this(t: Double, c: java.awt.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)
  }

  object RgbaList {
    def apply(rgba: Rgba): RgbaList = new RgbaList(Seq(rgba))

    def apply(r: Int, g: Int, b: Int, a: Int): RgbaList = new RgbaList(Rgba(r, g, b, a))

    def apply(t: String, r: Int, g: Int, b: Int, a: Int): RgbaList = new RgbaList(Rgba(TimeValue(t), r, g, b, a))

    def apply(t: Double, r: Int, g: Int, b: Int, a: Int): RgbaList = new RgbaList(Rgba(TimeValue(t), r, g, b, a))

    def apply(t: TimeValue, r: Int, g: Int, b: Int, a: Int): RgbaList = new RgbaList(Rgba(t, r, g, b, a))

    def apply(c: java.awt.Color): RgbaList = new RgbaList(c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def apply(t: String, c: java.awt.Color): RgbaList = new RgbaList(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    def apply(t: Double, c: java.awt.Color): RgbaList = new RgbaList(t, c.getRed, c.getGreen, c.getBlue, c.getAlpha)

    val theReads = new Reads[RgbaList] {
      def reads(js: JsValue): JsResult[RgbaList] = {
        val jsList = js.as[JsArray].value
        // have a list of timed values
        if (jsList.length >= 5 && (jsList.length % 5) == 0) {
          // note: to make reading more robust, reading Double then converting to Int
          JsSuccess(new RgbaList(
            (for (i <- jsList.indices by 5) yield ((JsPath \ i).readNullable[TimeValue] and
              (JsPath \ (i + 1)).read[Double].map(_.toInt) and (JsPath \ (i + 2)).read[Double].map(_.toInt) and
              (JsPath \ (i + 3)).read[Double].map(_.toInt) and (JsPath \ (i + 4)).read[Double].map(_.toInt)
              ) (Rgba.apply(_, _, _, _, _)).reads(js).asOpt).flatten))
        }
        // have a single color Rgba
        else {
          // note: to make reading more robust, reading Double then converting to Int
          val result = ((JsPath \ 0).read[Double].map(_.toInt) and (JsPath \ 1).read[Double].map(_.toInt) and
            (JsPath \ 2).read[Double].map(_.toInt) and (JsPath \ 3).read[Double].map(_.toInt)
            ) (Rgba.apply(None, _, _, _, _)).reads(js)
          result match {
            case s: JsSuccess[Rgba] => JsSuccess(new RgbaList(Seq(s.get)))
            case e: JsError =>
              logger.error("could not read rgba values: " + js.toString)
              JsError("could not read rgba values: " + js.toString)
          }
        }
      }
    }

    val theWrites = new Writes[RgbaList] {
      def writes(cart: RgbaList) = {
        val theList = for (value <- cart.values) yield {
          value.t match {
            case Some(time) => List(TimeValue.fmt.writes(time), JsNumber(value.r), JsNumber(value.g), JsNumber(value.b), JsNumber(value.a))
            case None => List(JsNumber(value.r), JsNumber(value.g), JsNumber(value.b), JsNumber(value.a))
          }
        }
        JsArray(theList.flatten)
      }
    }

    implicit val fmt: Format[RgbaList] = Format(theReads, theWrites)
  }

  /**
    * The color specified as an array of color components [Red, Green, Blue, Alpha]
    * where each component is a float in the range 0.0-1.0.
    */
  case class Rgbaf(t: Option[TimeValue] = None, r: Float, g: Float, b: Float, a: Float) {
    def this(t: TimeValue, r: Float, g: Float, b: Float, a: Float) = this(Option(t), r, g, b, a)

    def this(t: TimeValue, r: Double, g: Double, b: Double, a: Double) = this(t, r.toFloat, g.toFloat, b.toFloat, a.toFloat)

    def this(t: String, r: Float, g: Float, b: Float, a: Float) = this(TimeValue(t), r, g, b, a)

    def this(t: Double, r: Float, g: Float, b: Float, a: Float) = this(TimeValue(t), r, g, b, a)

    def this(t: String, r: Double, g: Double, b: Double, a: Double) = this(TimeValue(t), r, g, b, a)

    def this(t: Double, r: Double, g: Double, b: Double, a: Double) = this(TimeValue(t), r, g, b, a)

    def this(r: Double, g: Double, b: Double, a: Double) = this(None, r.toFloat, g.toFloat, b.toFloat, a.toFloat)

    def this(r: Float, g: Float, b: Float, a: Float) = this(None, r, g, b, a)

    def this(c: javafx.scene.paint.Color) = this(c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def this(t: String, c: javafx.scene.paint.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def this(t: Double, c: javafx.scene.paint.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)

  }

  object Rgbaf {
    implicit val fmt = Json.format[Rgbaf]

    def apply(t: TimeValue, r: Float, g: Float, b: Float, a: Float): Rgbaf = new Rgbaf(t, r, g, b, a)

    def apply(t: TimeValue, r: Double, g: Double, b: Double, a: Double): Rgbaf = new Rgbaf(t, r, g, b, a)

    def apply(t: String, r: Double, g: Double, b: Double, a: Double): Rgbaf = new Rgbaf(TimeValue(t), r, g, b, a)

    def apply(t: Double, r: Double, g: Double, b: Double, a: Double): Rgbaf = new Rgbaf(TimeValue(t), r, g, b, a)

    def apply(r: Double, g: Double, b: Double, a: Double): Rgbaf = new Rgbaf(r, g, b, a)

    def apply(t: String, r: Float, g: Float, b: Float, a: Float): Rgbaf = new Rgbaf(TimeValue(t), r, g, b, a)

    def apply(t: Double, r: Float, g: Float, b: Float, a: Float): Rgbaf = new Rgbaf(TimeValue(t), r, g, b, a)

    def apply(r: Float, g: Float, b: Float, a: Float): Rgbaf = new Rgbaf(r, g, b, a)

    def apply(c: javafx.scene.paint.Color): Rgbaf = new Rgbaf(c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def apply(t: String, c: javafx.scene.paint.Color): Rgbaf = new Rgbaf(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def apply(t: Double, c: javafx.scene.paint.Color): Rgbaf = new Rgbaf(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)
  }

  /**
    * A list of Rgbaf colors.
    */
  case class RgbafList(values: Seq[Rgbaf]) {
    def this(rgbaf: Rgbaf) = this(Seq(rgbaf))

    def this(r: Double, g: Double, b: Double, a: Double) = this(Rgbaf(r, g, b, a))

    def this(r: Float, g: Float, b: Float, a: Float) = this(Rgbaf(r, g, b, a))

    def this(t: TimeValue, r: Float, g: Float, b: Float, a: Float) = this(Rgbaf(Option(t), r, g, b, a))

    def this(t: TimeValue, r: Double, g: Double, b: Double, a: Double) = this(t, r.toFloat, g.toFloat, b.toFloat, a.toFloat)

    def this(t: String, r: Float, g: Float, b: Float, a: Float) = this(TimeValue(t), r, g, b, a)

    def this(t: Double, r: Float, g: Float, b: Float, a: Float) = this(TimeValue(t), r, g, b, a)

    def this(t: String, r: Double, g: Double, b: Double, a: Double) = this(TimeValue(t), r, g, b, a)

    def this(t: Double, r: Double, g: Double, b: Double, a: Double) = this(TimeValue(t), r, g, b, a)

    def this(c: javafx.scene.paint.Color) = this(c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def this(t: String, c: javafx.scene.paint.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def this(t: Double, c: javafx.scene.paint.Color) = this(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)

  }

  object RgbafList {
    def apply(rgbaf: Rgbaf): RgbafList = new RgbafList(Seq(rgbaf))

    def apply(r: Double, g: Double, b: Double, a: Double): RgbafList = new RgbafList(Rgbaf(r, g, b, a))

    def apply(r: Float, g: Float, b: Float, a: Float): RgbafList = new RgbafList(Rgbaf(r, g, b, a))

    def apply(t: TimeValue, r: Float, g: Float, b: Float, a: Float): RgbafList = new RgbafList(Rgbaf(Option(t), r, g, b, a))

    def apply(t: TimeValue, r: Double, g: Double, b: Double, a: Double): RgbafList = new RgbafList(t, r.toFloat, g.toFloat, b.toFloat, a.toFloat)

    def apply(t: String, r: Float, g: Float, b: Float, a: Float): RgbafList = new RgbafList(TimeValue(t), r, g, b, a)

    def apply(t: Double, r: Float, g: Float, b: Float, a: Float): RgbafList = new RgbafList(TimeValue(t), r, g, b, a)

    def apply(t: String, r: Double, g: Double, b: Double, a: Double): RgbafList = new RgbafList(TimeValue(t), r, g, b, a)

    def apply(t: Double, r: Double, g: Double, b: Double, a: Double): RgbafList = new RgbafList(TimeValue(t), r, g, b, a)

    def apply(c: javafx.scene.paint.Color): RgbafList = new RgbafList(c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def apply(t: String, c: javafx.scene.paint.Color): RgbafList = new RgbafList(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)

    def apply(t: Double, c: javafx.scene.paint.Color): RgbafList = new RgbafList(t, c.getRed, c.getGreen, c.getBlue, c.getOpacity)


    val theReads = new Reads[RgbafList] {
      def reads(js: JsValue): JsResult[RgbafList] = {
        val jsList = js.as[JsArray].value
        // todo fix this
        // have a list of timed values
        if (jsList.length >= 5 && (jsList.length % 5) == 0) {
          JsSuccess(new RgbafList(
            (for (i <- jsList.indices by 5) yield ((JsPath \ i).readNullable[TimeValue] and
              (JsPath \ (i + 1)).read[Float] and (JsPath \ (i + 2)).read[Float] and
              (JsPath \ (i + 3)).read[Float] and (JsPath \ (i + 4)).read[Float]
              ) (Rgbaf.apply(_, _, _, _, _)).reads(js).asOpt).flatten))
        }
        // have a single color Rgbaf
        else {
          val result = ((JsPath \ 0).read[Float] and (JsPath \ 1).read[Float] and
            (JsPath \ 2).read[Float] and (JsPath \ 3).read[Float]) (Rgbaf.apply(None, _, _, _, _)).reads(js)
          result match {
            case s: JsSuccess[Rgbaf] => JsSuccess(new RgbafList(Seq(s.get)))
            case e: JsError =>
              logger.error("could not read rgbaf values: " + js.toString)
              JsError("could not read rgbaf values: " + js.toString)
          }
        }
      }
    }

    val theWrites = new Writes[RgbafList] {
      def writes(cart: RgbafList) = {
        val theList = for (value <- cart.values) yield {
          value.t match {
            case Some(time) => List(TimeValue.fmt.writes(time), JsNumber(value.r), JsNumber(value.g), JsNumber(value.b), JsNumber(value.a))
            case None => List(JsNumber(value.r), JsNumber(value.g), JsNumber(value.b), JsNumber(value.a))
          }
        }
        JsArray(theList.flatten)
      }
    }

    implicit val fmt: Format[RgbafList] = Format(theReads, theWrites)
  }

  /**
    * a time and double value pair
    *
    * @param t the time component
    * @param v the value component
    */
  case class TimedDouble(t: Option[String] = None, v: Double) {
    def this(t: String, v: Double) = this(Option(t), v)

    def this(v: Double) = this(None, v)
  }

  object TimedDouble {
    implicit val fmt = Json.format[TimedDouble]

    def apply(v: Double): TimedDouble = new TimedDouble(v)

    def apply(t: String, v: Double): TimedDouble = new TimedDouble(t, v)
  }

  /**
    * represents a sequence of TimedDouble objects or a single instance of TimedDouble.
    *
    * @param values a sequence of TimedDouble objects.
    */
  case class TimedNumbers(values: Seq[TimedDouble]) {
    def this(v: Double) = this(Seq(new TimedDouble(None, v)))

    def this(t: String, v: Double) = this(Seq(new TimedDouble(t, v)))
  }

  object TimedNumbers {

    def apply(v: Double): TimedNumbers = new TimedNumbers(v)

    def apply(t: String, v: Double): TimedNumbers = new TimedNumbers(t, v)

    val theReads = new Reads[TimedNumbers] {
      def reads(js: JsValue): JsResult[TimedNumbers] = {

        // could be an array or a single number
        // try to read as a single number
        js.asOpt[JsNumber] match {
          // could have an array
          case None =>
            val jsList = js.as[JsArray].value
            if (jsList.length >= 2 && (jsList.length % 2) == 0) {
              // have a list of timed values
              JsSuccess(new TimedNumbers(
                (for (i <- jsList.indices by 2) yield
                  ((JsPath \ i).readNullable[String] and
                    (JsPath \ (i + 1)).read[Double]) (TimedDouble.apply(_, _)).reads(js).asOpt).flatten))

            } else {
              // have a single value in the array with no time component
              val result = (JsPath \ 0).read[Double].map(TimedDouble.apply(None, _)).reads(js)
              result match {
                case s: JsSuccess[TimedDouble] => JsSuccess(new TimedNumbers(Seq(s.get)))
                case e: JsError =>
                  logger.error("could not read TimedNumbers values: " + js.toString)
                  JsError("could not read TimedNumbers values: " + js.toString)
              }
            }

          // have a single value with no time component
          case Some(n) =>
            val result = JsPath.read[Double].map(TimedDouble.apply(None, _)).reads(js)
            result match {
              case s: JsSuccess[TimedDouble] => JsSuccess(new TimedNumbers(Seq(s.get)))
              case e: JsError =>
                logger.error("could not read TimedNumbers values: " + js.toString)
                JsError("could not read TimedNumbers values: " + js.toString)
            }
        }
      }
    }

    val theWrites = new Writes[TimedNumbers] {
      def writes(cart: TimedNumbers) = {
        val theList = for (value <- cart.values) yield {
          value.t match {
            case Some(time) => List(JsString(time), JsNumber(value.v))
            case None => List(JsNumber(value.v))
          }
        }
        if (theList.flatten.length == 1) theList.flatten.head else JsArray(theList.flatten)
      }
    }

    implicit val fmt: Format[TimedNumbers] = Format(theReads, theWrites)
  }

  /**
    * represents the horizontal origin "type"
    */
  sealed trait HORIZONTAL

  /**
    * represents the vertical origin "type"
    */
  sealed trait VERTICAL

  /**
    * The origin of the billboard or label.
    *
    * @param origin    the origin value, one of "LEFT", "CENTER", "RIGHT", "BOTTOM", "TOP"
    * @param reference a reference property
    * @tparam T the origin type/direction, either HORIZONTAL or VERTICAL
    */
  case class Origin[T: TypeTag](origin: Option[String] = None, reference: Option[String] = None) {
    def this(origin: String) = this(Option(origin))

    def this(origin: String, reference: String) = this(Option(origin), Option(reference))

    /**
      * true if this Origin is of the HORIZONTAL type
      */
    def isHorizontal() = typeOf[T] =:= typeOf[HORIZONTAL]

    /**
      * true if this Origin is of the VERTICAL type
      */
    def isVertical() = typeOf[T] =:= typeOf[VERTICAL]
  }

  object Origin {
    def apply[T: TypeTag](origin: String): Origin[T] = new Origin[T](origin)

    def apply[T: TypeTag](origin: String, reference: String): Origin[T] = new Origin[T](origin, reference)

    implicit def theReads[T: TypeTag] = new Reads[Origin[T]] {
      def reads(js: JsValue): JsResult[Origin[T]] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None =>
            if (typeOf[T] =:= typeOf[HORIZONTAL]) {
              JsSuccess(new Origin[T]((JsPath \ "horizontalOrigin").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))
            }
            else if (typeOf[T] =:= typeOf[VERTICAL]) {
              JsSuccess(new Origin[T]((JsPath \ "verticalOrigin").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))
            } else {
              logger.error("could not read the origin: " + js.toString())
              JsError("could not read the origin ")
            }

          case Some(hz) => JsSuccess(new Origin[T](Some(hz)))
        }
      }
    }

    implicit def theWrites[T: TypeTag] = new Writes[Origin[T]] {
      def writes(obj: Origin[T]) = {
        obj.reference match {
          case Some(ref) =>
            if (obj.isHorizontal()) {
              Json.obj("horizontalOrigin" -> JsString(obj.origin.getOrElse("")), "reference" -> JsString(ref))
            }
            if (obj.isVertical()) {
              Json.obj("verticalOrigin" -> JsString(obj.origin.getOrElse("")), "reference" -> JsString(ref))
            } else JsNull

          case None => JsString(obj.origin.getOrElse(""))
        }
      }
    }

  }

  /**
    * An HTML description of the object.
    *
    * @param string    The string value
    * @param reference A reference property.
    */
  case class Description(string: Option[String] = None, reference: Option[String] = None) {
    def this(string: String) = this(Option(string))
  }

  object Description {

    def apply(string: String): Description = new Description(string)

    val theReads = new Reads[Description] {
      def reads(js: JsValue): JsResult[Description] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new Description((JsPath \ "string").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(str) => JsSuccess(new Description(Some(str)))
        }
      }
    }

    val theWrites = new Writes[Description] {
      def writes(obj: Description) = {
        obj.reference match {
          case Some(ref) => Json.obj("string" -> JsString(obj.string.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.string.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[Description] = Format(theReads, theWrites)
  }

  /**
    * Fills the surface with an image. Used in Material
    *
    * @param image  The image to display on the surface.
    * @param repeat The number of times the image repeats along each axis.
    */
  case class Image(image: Option[ImageUri] = None, repeat: Option[CzmlCartesian2] = None) {
    def this(uri: String) = this(Option(ImageUri(uri)))

    def this(uri: String, x: Int, y: Int) = this(Option(ImageUri(uri)), Option(CzmlCartesian2(x, y)))

  }

  object Image {
    implicit val fmt = Json.format[Image]

    def apply(uri: String): Image = new Image(uri)

    def apply(uri: String, x: Int, y: Int): Image = new Image(uri, x, y)

  }

  /**
    * The image displayed on the billboard, expressed as a URL. For broadest client compatibility,
    * the URL should be accessible via Cross-Origin Resource Sharing (CORS). The URL may also be a data URI.
    *
    * @param uri       A URI value.  The URI can optionally vary with time.
    * @param reference A reference property.
    */
  case class ImageUri(uri: Option[String] = None, reference: Option[String] = None) {
    def this(uri: String) = this(Option(uri))

    def this(uri: String, reference: String) = this(Option(uri), Option(reference))
  }

  object ImageUri {

    def apply(uri: String): ImageUri = new ImageUri(uri)

    def apply(uri: String, reference: String): ImageUri = new ImageUri(uri, reference)

    val theReads = new Reads[ImageUri] {
      def reads(js: JsValue): JsResult[ImageUri] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new ImageUri((JsPath \ "uri").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(ur) => JsSuccess(new ImageUri(Some(ur)))
        }
      }
    }

    val theWrites = new Writes[ImageUri] {
      def writes(obj: ImageUri) = {
        obj.reference match {
          case Some(ref) => Json.obj("uri" -> JsString(obj.uri.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.uri.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[ImageUri] = Format(theReads, theWrites)
  }

  /**
    * Describes a 2d Cartesian property which can optionally vary over time.
    * Typically used for pixelOffset of a billboard or grid lineOffset, lineCount and lineThickness
    *
    * @param cartesian2 The Cartesian2D [X, Y] in meters. If the array has two elements, the cartesian is constant.
    *                   If it has three or more elements, they are time-tagged samples arranged as
    *                   [Time, X, Y, Time, X, Y, Time, X, Y,  ...], where Time is an ISO 8601 date and
    *                   time string or seconds since epoch.
    * @param interval   Time interval
    * @param reference  A reference property.
    * @param timeFields the time interpolatable part of this property
    */
  case class CzmlCartesian2(cartesian2: Option[Cartesian2D] = None,
                            interval: Option[String] = None,
                            reference: Option[String] = None,
                            timeFields: Option[Interpolatable] = None) {

    def this(cartesian2: Cartesian2D, interval: String) = this(Option(cartesian2), Option(interval))

    def this(x: Double, y: Double, interval: String) = this(Option(new Cartesian2D(x, y)), Option(interval))

    def this(x: Double, y: Double) = this(Option(new Cartesian2D(x, y)))

    def this(x: Int, y: Int) = this(Option(new Cartesian2D(x, y)))

    def this(cartesian2: Cartesian2D) = this(Option(cartesian2))

  }

  object CzmlCartesian2 {

    def apply(cartesian2: Cartesian2D, interval: String): CzmlCartesian2 = new CzmlCartesian2(cartesian2, interval)

    def apply(x: Double, y: Double, interval: String): CzmlCartesian2 = new CzmlCartesian2(x, y, interval)

    def apply(x: Double, y: Double): CzmlCartesian2 = new CzmlCartesian2(x, y)

    def apply(cartesian2: Cartesian2D): CzmlCartesian2 = new CzmlCartesian2(cartesian2)

    val theReads: Reads[CzmlCartesian2] =
      ((JsPath \ "cartesian2").readNullable[Cartesian2D] and
        (JsPath \ "interval").readNullable[String] and
        (JsPath \ "reference").readNullable[String] and
        Interpolatable.fmt) ((cart, intrv, ref, interpo) => CzmlCartesian2(cart, intrv, ref, Option(interpo)))

    val theWrites: Writes[CzmlCartesian2] =
      ((JsPath \ "cartesian2").writeNullable[Cartesian2D] and
        (JsPath \ "interval").writeNullable[String] and
        (JsPath \ "reference").writeNullable[String] and
        JsPath.writeNullable[Interpolatable]) (unlift(CzmlCartesian2.unapply))

    implicit val fmt: Format[CzmlCartesian2] = Format(theReads, theWrites)
  }

  /**
    * A generic number that can be timed and is interpolatable.
    *
    * Note: if epoch is defined "number" becomes an array of doubles.
    * if epoch is not defined, then "number" becomes either an array of timed values (see TimedNumbers) or
    * a single double value.
    *
    * @param number     the number that depends on epoch
    * @param interval   the time interval value
    * @param reference  a reference property
    * @param timeFields the time interpolatable part of this property
    */
  case class CzmlNumber(number: Option[Any] = None, interval: Option[String] = None,
                        reference: Option[String] = None,
                        timeFields: Option[Interpolatable] = None) {

    def this(number: Double) = this(Option(number))

    def this(number: Double, interval: String) = this(Option(number), Option(interval))

    def this(number: TimedNumbers) = this(Option(number))

    def this(number: Array[Double], epoch: String) = this(Option(number), None, Option(epoch))
  }

  object CzmlNumber {

    def apply(number: Double): CzmlNumber = new CzmlNumber(number)

    def apply(number: Double, interval: String): CzmlNumber = new CzmlNumber(number, interval)

    def apply(number: TimedNumbers): CzmlNumber = new CzmlNumber(number)

    def apply(number: Array[Double], epoch: String): CzmlNumber = new CzmlNumber(number, epoch)

    val theReads = new Reads[CzmlNumber] {
      def reads(js: JsValue): JsResult[CzmlNumber] = {
        // read the full set of fields
        def doRead(): JsResult[CzmlNumber] = {
          val interv = (JsPath \ "interval").read[String].reads(js).asOpt
          val ref = (JsPath \ "reference").read[String].reads(js).asOpt
          val interpo = Interpolatable.fmt.reads(js).asOpt
          interpo match {
            case Some(p) =>
              val number = p.epoch match {
                // with epoch -> number field could be an array of doubles
                case Some(epok) =>
                  (js \ "number").asOpt[Array[Double]] match {
                    case None => (js \ "number").asOpt[Double]
                    case Some(arr) => Some(arr)
                  }
                // no epoch -> number field could be a list of TimedNumbers
                case None => (JsPath \ "number").read[TimedNumbers].reads(js).asOpt
              }
              JsSuccess(new CzmlNumber(number, interv, ref, interpo))

            case None => JsSuccess(new CzmlNumber(None, interv, ref, interpo))
          }
        }

        // check that we could have a simple double
        JsPath.read[Double].reads(js).asOpt match {
          case Some(n) => JsSuccess(new CzmlNumber(Some(n)))
          case None => doRead()
        }
      }
    }

    val theWrites = new Writes[CzmlNumber] {
      def writes(czmlN: CzmlNumber) = {

        val theList = ListBuffer[Option[(String, JsValue)]](
          czmlN.reference.map("reference" -> JsString(_)),
          czmlN.interval.map("interval" -> JsString(_)))

        czmlN.timeFields.map(p =>
          theList ++= ListBuffer(
            p.epoch.map("epoch" -> JsString(_)),
            p.nextTime.map("nextTime" -> TimeValue.fmt.writes(_)),
            p.previousTime.map("previousTime" -> TimeValue.fmt.writes(_)),
            p.interpolationAlgorithm.map("interpolationAlgorithm" -> JsString(_)),
            p.interpolationDegree.map("interpolationDegree" -> JsNumber(_)),
            p.forwardExtrapolationType.map("forwardExtrapolationType" -> JsString(_)),
            p.forwardExtrapolationDuration.map("forwardExtrapolationDuration" -> JsNumber(_)),
            p.backwardExtrapolationType.map("backwardExtrapolationType" -> JsString(_)),
            p.backwardExtrapolationDuration.map("backwardExtrapolationDuration" -> JsNumber(_))))

        def doWrite() = {
          czmlN.timeFields map (p => {
            // number could be written as a JsArray or a JsNumber depending on epoch
            val number = p.epoch match {
              // no epoch the number field could be a list of TimedNumbers (which itself can be a single double)
              case None => czmlN.number.map(x =>
                if (x.isInstanceOf[TimedNumbers]) TimedNumbers.fmt.writes(x.asInstanceOf[TimedNumbers]))

              // with an epoch the number field could be an array of doubles or a single double
              case Some(epok) => czmlN.number.map({
                case v: Array[Double] => Json.toJson(v)
                case v: Double => JsNumber(v)
              })
            }
            // add the number to theList
            number match {
              case Some(x: JsNumber) => theList += Option("number" -> x)
              case Some(x: JsArray) => theList += Option("number" -> x)
              case _ => JsNull
            }
          })
          JsObject(theList.flatten)
        }

        theList.flatten.isEmpty match {
          // have a simple number
          case true =>
            czmlN.number match {
              case Some(x: TimedNumbers) => JsNumber(x.values.head.v)
              case Some(x: Double) => Json.obj("number" -> JsNumber(x))
              case _ => JsNull
            }
          // have complex property
          case false => doWrite()
        }
      }
    }
    implicit val fmt: Format[CzmlNumber] = Format(theReads, theWrites)
  }

  /**
    * A number property consisting of a possible list of CzmlNumber
    *
    * @param values the list of CzmlNumbers
    */
  case class Number(values: Option[Array[CzmlNumber]]) {
    def this(values: Array[CzmlNumber]) = this(Option(values))

    def this(number: Double) = this(Option(Array(CzmlNumber(number))))

    def this(number: CzmlNumber) = this(Option(Array(number)))

    def this(number: Double, interval: String) = this(CzmlNumber(number, interval))

    def this(number: Array[Double], epoch: String) = this(CzmlNumber(number, epoch))

  }

  object Number {

    def apply(values: Array[CzmlNumber]): Number = new Number(values)

    def apply(number: Double): Number = new Number(number)

    def apply(number: CzmlNumber): Number = new Number(number)

    def apply(number: Array[Double], epoch: String): Number = new Number(number, epoch)

    def apply(number: Double, interval: String): Number = new Number(number, interval)


    val theReads = new Reads[Number] {
      def reads(js: JsValue): JsResult[Number] = {
        JsPath.read[Array[CzmlNumber]].reads(js).asOpt match {
          // have a single property that we wrap in an option array
          case None => JsSuccess(new Number(Option(Array(JsPath.read[CzmlNumber].reads(js).getOrElse(CzmlNumber(None))))))
          // have an array of properties
          case Some(b) => JsSuccess(new Number(Some(b)))
        }
      }
    }

    val theWrites = new Writes[Number] {
      def writes(propList: Number) = {
        propList.values match {
          case None => JsNull
          case Some(list) =>
            if (list.length == 1) CzmlNumber.fmt.writes(list.head)
            else Json.toJson(list)
        }
      }
    }

    implicit val fmt: Format[Number] = Format(theReads, theWrites)
  }


  /**
    * Defines a color property. The color can optionally vary over time.
    *
    * @param rgba       A color specified as an array of color components [Red, Green, Blue, Alpha]
    *                   where each component is in the range 0-255. If the array has four elements,
    *                   the color is constant. If it has five or more elements,
    *                   they are time-tagged samples arranged as
    *                   [Time, Red, Green, Blue, Alpha, Time, Red, Green, Blue, Alpha, ...],
    *                   where Time is an ISO 8601 date and time string or seconds since epoch.
    * @param rgbaf      The color specified as an array of color components [Red, Green, Blue, Alpha]
    *                   where each component is in the range 0.0-1.0. If the array has four elements,
    *                   the color is constant. If it has five or more elements, they are time-tagged samples
    *                   arranged as [Time, Red, Green, Blue, Alpha, Time, Red, Green, Blue, Alpha, ...],
    *                   where Time is an ISO 8601 date and time string or seconds since epoch.
    * @param interval   the interval property
    * @param reference  a reference property
    * @param timeFields the time interpolatable part of this property
    */
  case class CzmlColor(rgba: Option[RgbaList] = None, rgbaf: Option[RgbafList] = None,
                       interval: Option[String] = None,
                       reference: Option[String] = None,
                       timeFields: Option[Interpolatable] = None) {

    def this(rgba: RgbaList, interval: String) = this(Option(rgba), None, Option(interval))

    def this(rgba: RgbaList) = this(Option(rgba))

    def this(rgbaf: RgbafList, interval: String) = this(None, Option(rgbaf), None, Option(interval))

    def this(rgbaf: RgbafList) = this(None, Option(rgbaf))

    def this(rgba: Rgba) = this(new RgbaList(rgba))

    def this(rgba: Rgba, interval: String) = this(new RgbaList(rgba), interval)

    def this(rgbaf: Rgbaf) = this(None, Option(new RgbafList(rgbaf)))

    def this(rgbaf: Rgbaf, interval: String) = this(None, Option(new RgbafList(rgbaf)), None, Option(interval))

    def this(c: javafx.scene.paint.Color) = this(Rgbaf(c.getRed, c.getGreen, c.getBlue, c.getOpacity))

    def this(c: java.awt.Color) = this(Rgba(c.getRed, c.getGreen, c.getBlue, c.getAlpha))

  }

  object CzmlColor {

    def apply(rgba: RgbaList, interval: String): CzmlColor = new CzmlColor(rgba, interval)

    def apply(rgba: RgbaList): CzmlColor = new CzmlColor(rgba)

    def apply(rgbaf: RgbafList, interval: String): CzmlColor = new CzmlColor(rgbaf, interval)

    def apply(rgbaf: RgbafList): CzmlColor = new CzmlColor(rgbaf)

    def apply(rgba: Rgba, interval: String): CzmlColor = new CzmlColor(rgba, interval)

    def apply(rgba: Rgba): CzmlColor = new CzmlColor(rgba)

    def apply(rgbaf: Rgbaf, interval: String): CzmlColor = new CzmlColor(rgbaf, interval)

    def apply(rgbaf: Rgbaf): CzmlColor = new CzmlColor(rgbaf)

    def apply(c: javafx.scene.paint.Color): CzmlColor = new CzmlColor(c)

    def apply(c: java.awt.Color): CzmlColor = new CzmlColor(c)

    val theReads: Reads[CzmlColor] =
      ((JsPath \ "rgba").readNullable[RgbaList] and
        (JsPath \ "rgbaf").readNullable[RgbafList] and
        (JsPath \ "interval").readNullable[String] and
        (JsPath \ "reference").readNullable[String] and
        Interpolatable.fmt) ((rgba, rgbaf, intrv, ref, interpo) => CzmlColor(rgba, rgbaf, intrv, ref, Option(interpo)))

    val theWrites: Writes[CzmlColor] =
      ((JsPath \ "rgba").writeNullable[RgbaList] and
        (JsPath \ "rgbaf").writeNullable[RgbafList] and
        (JsPath \ "interval").writeNullable[String] and
        (JsPath \ "reference").writeNullable[String] and
        JsPath.writeNullable[Interpolatable]) (unlift(CzmlColor.unapply))

    implicit val fmt: Format[CzmlColor] = Format(theReads, theWrites)
  }

  /**
    * A color property represented as an array of color objects
    *
    * @param values the array of Colors
    */
  case class ColorProperty(values: Option[Array[CzmlColor]]) {
    def this(values: Array[CzmlColor]) = this(Option(values))

    def this(color: CzmlColor) = this(Option(Array(color)))

    def this(rgba: Rgba) = this(Option(Array(new CzmlColor(new RgbaList(rgba)))))

    def this(r: Int, g: Int, b: Int, a: Int) = this(Option(Array(new CzmlColor(new RgbaList(new Rgba(r, g, b, a))))))

    def this(rgbaf: Rgbaf) = this(Option(Array(new CzmlColor(new RgbafList(rgbaf)))))

    def this(r: Float, g: Float, b: Float, a: Float) = this(Option(Array(new CzmlColor(new RgbafList(new Rgbaf(r, g, b, a))))))

    def this(r: Double, g: Double, b: Double, a: Double) = this(Option(Array(new CzmlColor(new RgbafList(new Rgbaf(r, g, b, a))))))

    def this(c: javafx.scene.paint.Color) = this(CzmlColor(c))

    def this(c: java.awt.Color) = this(CzmlColor(c))

  }

  object ColorProperty {

    def apply(values: Array[CzmlColor]): ColorProperty = new ColorProperty(values)

    def apply(color: CzmlColor): ColorProperty = new ColorProperty(color)

    def apply(rgba: Rgba): ColorProperty = new ColorProperty(rgba)

    def apply(r: Int, g: Int, b: Int, a: Int): ColorProperty = new ColorProperty(r, g, b, a)

    def apply(rgbaf: Rgbaf): ColorProperty = new ColorProperty(rgbaf)

    def apply(r: Float, g: Float, b: Float, a: Float): ColorProperty = new ColorProperty(r, g, b, a)

    def apply(r: Double, g: Double, b: Double, a: Double): ColorProperty = new ColorProperty(r, g, b, a)

    def apply(c: javafx.scene.paint.Color) = new ColorProperty(CzmlColor(c))

    def apply(c: java.awt.Color) = new ColorProperty(CzmlColor(c))

    val theReads = new Reads[ColorProperty] {
      def reads(js: JsValue): JsResult[ColorProperty] = {
        JsPath.read[Array[CzmlColor]].reads(js).asOpt match {
          // have a single property that we wrap in an option array
          case None => JsSuccess(new ColorProperty(Option(Array(JsPath.read[CzmlColor].reads(js).getOrElse(CzmlColor(None))))))
          // have an array of properties
          case Some(b) => JsSuccess(new ColorProperty(Some(b)))
        }
      }
    }

    val theWrites = new Writes[ColorProperty] {
      def writes(propList: ColorProperty) = {
        propList.values match {
          case None => JsNull
          case Some(list) =>
            if (list.length == 1) CzmlColor.fmt.writes(list.head)
            else Json.toJson(list)
        }
      }
    }

    implicit val fmt: Format[ColorProperty] = Format(theReads, theWrites)
  }

  /**
    * a font
    */
  case class Font(font: Option[String] = None, reference: Option[String] = None) {
    def this(font: String) = this(Option(font))

    def this(font: String, reference: String) = this(Option(font), Option(reference))
  }

  object Font {

    def apply(font: String): Font = new Font(font)

    def apply(font: String, reference: String): Font = new Font(font, reference)

    val theReads = new Reads[Font] {
      def reads(js: JsValue): JsResult[Font] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new Font((JsPath \ "font").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(f) => JsSuccess(new Font(Some(f)))
        }
      }
    }

    val theWrites = new Writes[Font] {
      def writes(obj: Font) = {
        obj.reference match {
          case Some(ref) => Json.obj("font" -> JsString(obj.font.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.font.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[Font] = Format(theReads, theWrites)
  }

  /**
    * describes a string value with a possible time interval for use in Text
    */
  case class StringInterval(interval: Option[String] = None, string: Option[String]) {
    def this(interval: String, string: String) = this(Option(interval), Option(string))
  }

  object StringInterval {
    implicit val fmt = Json.format[StringInterval]

    def apply(interval: String, string: String): StringInterval = new StringInterval(interval, string)
  }

  /**
    * a text property for a Label, can include a "interval"
    *
    * @param string    either a simple string for the text or an array of (interval,string)
    * @param reference a reference property
    */
  case class Text(string: Either[String, Array[StringInterval]], reference: Option[String] = None) {

    def this(string: String, reference: String) = this(Left(string), Option(reference))

    def this(string: Array[StringInterval], reference: String) = this(Right(string), Option(reference))

    def this(string: String) = this(Left(string))

    def this(string: Array[StringInterval]) = this(Right(string))

    def this(string: StringInterval) = this(Right(Array(string)))

    def this(string: String, interval: String, reference: String) = this(Right(Array(new StringInterval(interval, string))), Option(reference))

  }

  object Text {

    def apply(string: String): Text = new Text(string)

    def apply(string: String, reference: String): Text = new Text(string, reference)

    val theReads = new Reads[Text] {
      def reads(js: JsValue): JsResult[Text] = {
        // try to read the string field
        val result = (JsPath \ "string").read[String].reads(js).asOpt match {
          case Some(b) => Left(b)
          case None =>
            // try to read a simple String
            JsPath.read[String].reads(js).asOpt match {
              case None => Right(JsPath.read[Array[StringInterval]].reads(js).getOrElse(Array[StringInterval]()))
              case Some(b) => Left(b)
            }
        }
        val ref = (JsPath \ "reference").read[String].reads(js).asOpt
        JsSuccess(new Text(result, ref))
      }
    }

    val theWrites = new Writes[Text] {
      def writes(txt: Text) = {
        val theText = txt.string match {
          case Left(x) => JsString(x)
          case Right(x) => Json.toJson(x)
        }
        txt.reference match {
          case Some(ref) => Json.obj("string" -> theText, "reference" -> JsString(ref))
          case None => theText
        }
      }
    }

    implicit val fmt: Format[Text] = Format(theReads, theWrites)
  }

  /**
    * The style of a label.
    *
    * @param labelStyle "FILL", "OUTLINE", and "FILL_AND_OUTLINE"
    * @param reference  A reference property
    */
  case class Style(labelStyle: Option[String] = None, reference: Option[String] = None) {

    def this(labelStyle: String) = this(Option(labelStyle))

    def this(labelStyle: String, reference: String) = this(Option(labelStyle), Option(reference))
  }

  object Style {

    def apply(labelStyle: String): Style = new Style(labelStyle)

    def apply(labelStyle: String, reference: String): Style = new Style(labelStyle, reference)


    val theReads = new Reads[Style] {
      def reads(js: JsValue): JsResult[Style] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new Style((JsPath \ "labelStyle").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(s) => JsSuccess(new Style(Some(s)))
        }
      }
    }

    val theWrites = new Writes[Style] {
      def writes(obj: Style) = {
        obj.reference match {
          case Some(ref) => Json.obj("labelStyle" -> JsString(obj.labelStyle.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.labelStyle.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[Style] = Format(theReads, theWrites)
  }

  /**
    * The position of the object in the world. The position has no direct visual representation,
    * but it is used to locate billboards, labels, and other primitives attached to the object.
    * It is also used for the scale of a NodeTransformation
    *
    * @param referenceFrame      the reference frame
    * @param cartesian           the cartesian position
    * @param cartographicRadians the radian position
    * @param cartographicDegrees the degree position
    * @param cartesianVelocity   the cartesian velocity
    * @param interval            the interval property
    * @param reference           a reference property
    * @param timeFields          the time interpolatable part of this property
    */
  case class CzmlPosition(referenceFrame: Option[String] = None, cartesian: Option[Cartesian] = None,
                          cartographicRadians: Option[Cartographic[RADIAN]] = None,
                          cartographicDegrees: Option[Cartographic[DEGREE]] = None,
                          cartesianVelocity: Option[CartesianVelocity] = None,
                          interval: Option[String] = None,
                          reference: Option[String] = None,
                          timeFields: Option[Interpolatable] = None) {

    def this(referenceFrame: String, cartesian: Cartesian, interval: String) = this(Option(referenceFrame), Option(cartesian), None, None, None, Option(interval))

    def this(referenceFrame: String, x: Double, y: Double, z: Double, interval: String) = this(Option(referenceFrame), Option(new Cartesian(x, y, z)), None, None, None, Option(interval))

    def this(x: Double, y: Double, z: Double) = this(None, Option(new Cartesian(x, y, z)))

    def this(cartesian: Cartesian) = this(None, Option(cartesian))

    def this(referenceFrame: String, x: Double, y: Double, z: Double) = this(Option(referenceFrame), Option(new Cartesian(x, y, z)))

  }

  object CzmlPosition {

    def apply(referenceFrame: String, cartesian: Cartesian, interval: String): CzmlPosition = new CzmlPosition(referenceFrame, cartesian, interval)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double, interval: String): CzmlPosition = new CzmlPosition(referenceFrame, x, y, z, interval)

    def apply(x: Double, y: Double, z: Double): CzmlPosition = new CzmlPosition(x, y, z)

    def apply(cartesian: Cartesian): CzmlPosition = new CzmlPosition(cartesian)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double): CzmlPosition = new CzmlPosition(referenceFrame, x, y, z)

    val theReads: Reads[CzmlPosition] =
      ((JsPath \ "referenceFrame").readNullable[String] and
        (JsPath \ "cartesian").readNullable[Cartesian] and
        (JsPath \ "cartographicRadians").readNullable[Cartographic[RADIAN]] and
        (JsPath \ "cartographicDegrees").readNullable[Cartographic[DEGREE]] and
        (JsPath \ "cartesianVelocity").readNullable[CartesianVelocity] and
        (JsPath \ "interval").readNullable[String] and
        (JsPath \ "reference").readNullable[String] and
        Interpolatable.fmt) ((reff, cart, rad, deg, v, intrv, ref, interpo) => CzmlPosition(reff, cart, rad, deg, v, intrv, ref, Option(interpo)))

    val theWrites: Writes[CzmlPosition] =
      ((JsPath \ "referenceFrame").writeNullable[String] and
        (JsPath \ "cartesian").writeNullable[Cartesian] and
        (JsPath \ "cartographicRadians").writeNullable[Cartographic[RADIAN]] and
        (JsPath \ "cartographicDegrees").writeNullable[Cartographic[DEGREE]] and
        (JsPath \ "cartesianVelocity").writeNullable[CartesianVelocity] and
        (JsPath \ "interval").writeNullable[String] and
        (JsPath \ "reference").writeNullable[String] and
        JsPath.writeNullable[Interpolatable]) (unlift(CzmlPosition.unapply))

    implicit val fmt: Format[CzmlPosition] = Format(theReads, theWrites)
  }

  /**
    * A non-timed/non-interpolatable value position, typically used to define a geometry, such as:
    * Polyline, Wall and Polygon
    */
  case class Position(referenceFrame: Option[String] = None, cartesian: Option[Cartesian] = None,
                      cartographicRadians: Option[Array[Double]] = None,
                      cartographicDegrees: Option[Array[Double]] = None,
                      references: Option[Array[String]] = None,
                      interval: Option[String] = None) {

    def this(cartesian: Cartesian) = this(cartesian = Option(cartesian))

    def this(x: Double, y: Double, z: Double) = this(cartesian = Cartesian(x, y, z))

    def this(t: String, x: Double, y: Double, z: Double) = this(cartesian = Cartesian(t, x, y, z))

    def this(t: Double, x: Double, y: Double, z: Double) = this(cartesian = Cartesian(t, x, y, z))

    def this(t: TimeValue, x: Double, y: Double, z: Double) = this(cartesian = Cartesian(t, x, y, z))

    def this(cartographicDegrees: Array[Double]) = this(cartographicDegrees = Option(cartographicDegrees))

  }

  object Position {

    implicit val fmt = Json.format[Position]

    def apply(cartesian: Cartesian): Position = new Position(cartesian)

    def apply(x: Double, y: Double, z: Double): Position = new Position(x, y, z)

    def apply(t: String, x: Double, y: Double, z: Double): Position = new Position(t, x, y, z)

    def apply(t: Double, x: Double, y: Double, z: Double): Position = new Position(t, x, y, z)

    def apply(t: TimeValue, x: Double, y: Double, z: Double): Position = new Position(t, x, y, z)

    def apply(cartographicDegrees: Array[Double]): Position = new Position(cartographicDegrees = Option(cartographicDegrees))
  }

  /**
    * A property for an array of Position, typically used to define a geometry, such as:
    * Polyline, Wall and Polygon
    *
    * @param values the array of Position
    */
  case class Positions(values: Option[Array[Position]]) {
    def this(values: Array[Position]) = this(Option(values))

    def this(positions: Position) = this(Option(Array(positions)))

    def this(cartesian: Cartesian) = this(Position(cartesian))

    def this(x: Double, y: Double, z: Double) = this(Position(x, y, z))

    def this(cartographicDegrees: Array[Double]) = this(Position(cartographicDegrees = Option(cartographicDegrees)))

  }

  object Positions {
    def apply(cartesian: Cartesian): Positions = new Positions(cartesian)

    def apply(positions: Position): Positions = new Positions(Option(Array(positions)))

    def apply(x: Double, y: Double, z: Double): Positions = new Positions(x, y, z)

    def apply(cartographicDegrees: Array[Double]): Positions = new Positions(cartographicDegrees)

    val theReads = new Reads[Positions] {
      def reads(js: JsValue): JsResult[Positions] = {
        JsPath.read[Array[Position]].reads(js).asOpt match {
          // have a single property that we wrap in an option array
          case None => JsSuccess(new Positions(Option(Array(JsPath.read[Position].reads(js).getOrElse(Position(None))))))
          // have an array of properties
          case Some(b) => JsSuccess(new Positions(Some(b)))
        }
      }
    }

    val theWrites = new Writes[Positions] {
      def writes(propList: Positions) = {
        propList.values match {
          case None => JsNull
          case Some(list) =>
            if (list.length == 1) Position.fmt.writes(list.head)
            else Json.toJson(list)
        }
      }
    }

    implicit val fmt: Format[Positions] = Format(theReads, theWrites)
  }

  /**
    * represents the orientation of the stripes, either "HORIZONTAL" or "VERTICAL"
    *
    * @param stripeOrientation "HORIZONTAL" or "VERTICAL"
    * @param reference         a reference property
    */
  case class StripeOrientation(stripeOrientation: Option[String] = None, reference: Option[String] = None) {
    def this(stripeOrientation: String) = this(Option(stripeOrientation))

    def this(stripeOrientation: String, reference: String) = this(Option(stripeOrientation), Option(reference))
  }

  object StripeOrientation {

    def apply(stripeOrientation: String): StripeOrientation = new StripeOrientation(stripeOrientation)

    def apply(stripeOrientation: String, reference: String): StripeOrientation = new StripeOrientation(stripeOrientation, reference)

    val theReads = new Reads[StripeOrientation] {
      def reads(js: JsValue): JsResult[StripeOrientation] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new StripeOrientation((JsPath \ "stripeOrientation").read[String].reads(js).asOpt,
              (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(hz) => JsSuccess(new StripeOrientation(Some(hz)))
        }
      }
    }

    val theWrites = new Writes[StripeOrientation] {
      def writes(obj: StripeOrientation) = {
        obj.reference match {
          case Some(ref) => Json.obj("stripeOrientation" -> JsString(obj.stripeOrientation.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.stripeOrientation.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[StripeOrientation] = Format(theReads, theWrites)
  }

  /**
    * to fill a surface with alternating colors.
    *
    * @param orientation "HORIZONTAL" or "VERTICAL"
    * @param evenColor   the even stripe color
    * @param oddColor    the odd stripe color
    * @param offset      The value indicating where in the pattern to begin drawing; with 0.0 being the beginning
    *                    of the even color, 1.0 the beginning of the odd color, 2.0 being the even color again,
    *                    and any multiple or fractional values being in between.
    * @param repeat      The number of time the stripes repeat.
    */
  case class Stripe(orientation: Option[StripeOrientation] = None, evenColor: Option[ColorProperty] = None,
                    oddColor: Option[ColorProperty] = None, offset: Option[Number] = None,
                    repeat: Option[Number] = None) {

    def this(orientation: String, evenColor: CzmlColor, oddColor: CzmlColor, offset: Double, repeat: Int) =
      this(Option(new StripeOrientation(orientation)), Option(new ColorProperty(evenColor)),
        Option(new ColorProperty(oddColor)), Option(new Number(offset)), Option(new Number(repeat)))

  }

  object Stripe {
    implicit val fmt = Json.format[Stripe]

    def apply(orientation: String, evenColor: CzmlColor, oddColor: CzmlColor, offset: Double, repeat: Int): Stripe =
      new Stripe(orientation, evenColor, oddColor, offset, repeat)
  }

  /**
    * Fills a surface with a grid.
    *
    * @param color         The color of the surface.
    * @param cellAlpha     Alpha value for the space between grid lines. This will be combined with the color alpha.
    * @param lineCount     The number of grid lines along each axis.
    * @param lineThickness The thickness of grid lines along each axis, in pixels.
    * @param lineOffset    The offset of grid lines along each axis, as a percentage from 0 to 1.
    */
  case class Grid(color: Option[ColorProperty] = None, cellAlpha: Option[Number] = None,
                  lineCount: Option[CzmlCartesian2] = None, lineThickness: Option[CzmlCartesian2] = None,
                  lineOffset: Option[CzmlCartesian2] = None) {

    def this(color: CzmlColor, cellAlpha: Double,
             lineCount: Cartesian2D, lineThickness: Cartesian2D, lineOffset: Cartesian2D) =
      this(Option(new ColorProperty(color)), Option(new Number(cellAlpha)),
        Option(new CzmlCartesian2(lineCount)), Option(new CzmlCartesian2(lineThickness)), Option(new CzmlCartesian2(lineOffset)))
  }

  object Grid {
    implicit val fmt = Json.format[Grid]

    def apply(color: CzmlColor, cellAlpha: Double, lineCount: Cartesian2D, lineThickness: Cartesian2D, lineOffset: Cartesian2D): Grid =
      new Grid(color, cellAlpha, lineCount, lineThickness, lineOffset)
  }

  /**
    * Fills the surface with a solid color, which may be translucent.
    *
    * @param color the color to fill the surface with
    */
  case class SolidColor(color: Option[ColorProperty] = None) {
    def this(color: ColorProperty) = this(Option(color))

    def this(color: CzmlColor) = this(Option(new ColorProperty(color)))

    def this(rgba: Rgba) = this(new CzmlColor(new RgbaList(rgba)))

    def this(r: Int, g: Int, b: Int, a: Int) = this(new Rgba(r, g, b, a))

    def this(rgbaf: Rgbaf) = this(new CzmlColor(rgbaf))

    def this(r: Float, g: Float, b: Float, a: Float) = this(new Rgbaf(r, g, b, a))

    def this(r: Double, g: Double, b: Double, a: Double) = this(new Rgbaf(r, g, b, a))

    def this(color: javafx.scene.paint.Color) = this(CzmlColor(color))

    def this(color: java.awt.Color) = this(CzmlColor(color))
  }

  object SolidColor {
    implicit val fmt = Json.format[SolidColor]

    def apply(color: ColorProperty): SolidColor = new SolidColor(color)

    def apply(color: CzmlColor): SolidColor = new SolidColor(color)

    def apply(rgba: Rgba): SolidColor = new SolidColor(rgba)

    def apply(r: Int, g: Int, b: Int, a: Int): SolidColor = new SolidColor(r, g, b, a)

    def apply(rgbaf: Rgbaf): SolidColor = new SolidColor(rgbaf)

    def apply(r: Float, g: Float, b: Float, a: Float): SolidColor = new SolidColor(r, g, b, a)

    def apply(r: Double, g: Double, b: Double, a: Double): SolidColor = new SolidColor(r, g, b, a)

    def apply(color: javafx.scene.paint.Color): SolidColor = new SolidColor(color)

    def apply(color: java.awt.Color): SolidColor = new SolidColor(color)
  }

  /**
    * The material to use to fill the ellipse.
    *
    * @param solidColor fill the surface with a solid color, which may be translucent.
    * @param image      fill the surface with an image
    * @param grid       fill the surface with a gird
    * @param stripe     fill the surface with a stripe
    */
  case class Material(solidColor: Option[SolidColor] = None, image: Option[Image] = None,
                      grid: Option[Grid] = None, stripe: Option[Stripe] = None) {

    def this(solidColor: ColorProperty) = this(solidColor = Option(SolidColor(solidColor)))

    def this(solidColor: CzmlColor) = this(solidColor = Option(SolidColor(solidColor)))

    def this(rgba: Rgba) = this(solidColor = Option(SolidColor(rgba)))

    def this(r: Int, g: Int, b: Int, a: Int) = this(solidColor = Option(SolidColor(r, g, b, a)))

    def this(rgbaf: Rgbaf) = this(solidColor = Option(SolidColor(rgbaf)))

    def this(r: Float, g: Float, b: Float, a: Float) = this(solidColor = Option(SolidColor(r, g, b, a)))

    def this(r: Double, g: Double, b: Double, a: Double) = this(solidColor = Option(SolidColor(r, g, b, a)))

    def this(image: Image) = this(image = Option(image))

    def this(uri: String) = this(image = Option(Image(uri)))

    def this(color: javafx.scene.paint.Color) = this(CzmlColor(color))

    def this(color: java.awt.Color) = this(CzmlColor(color))

  }

  object Material {
    implicit val fmt = Json.format[Material]

    def apply(color: ColorProperty): Material = new Material(color)

    def apply(color: CzmlColor): Material = new Material(color)

    def apply(rgba: Rgba): Material = new Material(rgba)

    def apply(r: Int, g: Int, b: Int, a: Int): Material = new Material(r, g, b, a)

    def apply(rgbaf: Rgbaf): Material = new Material(rgbaf)

    def apply(r: Float, g: Float, b: Float, a: Float): Material = new Material(r, g, b, a)

    def apply(r: Double, g: Double, b: Double, a: Double): Material = new Material(r, g, b, a)

    def apply(image: Image): Material = new Material(image)

    def apply(uri: String): Material = new Material(uri)

    def apply(color: javafx.scene.paint.Color): Material = new Material(color)

    def apply(color: java.awt.Color): Material = new Material(color)

  }

  /**
    * Colors the line with a glowing color.
    *
    * @param color     the color of glow
    * @param glowPower the strenght of the glow
    */
  case class PolylineGlow(color: Option[ColorProperty] = None, glowPower: Option[Number] = None) {
    def this(color: ColorProperty, glowPower: Number) = this(Option(color), Option(glowPower))

    def this(color: CzmlColor, glowPower: Double) = this(Option(new ColorProperty(color)), Option(new Number(glowPower)))

    def this(rgba: Rgba, glowPower: Double) = this(Option(new ColorProperty(rgba)), Option(new Number(glowPower)))

    def this(r: Int, g: Int, b: Int, a: Int, glowPower: Double) = this(Option(new ColorProperty(new Rgba(r, g, b, a))), Option(new Number(glowPower)))

    def this(rgbaf: Rgbaf, glowPower: Double) = this(Option(new ColorProperty(rgbaf)), Option(new Number(glowPower)))

    def this(r: Double, g: Double, b: Double, a: Double, glowPower: Double) = this(Option(new ColorProperty(new Rgbaf(r, g, b, a))), Option(new Number(glowPower)))

    def this(color: CzmlColor) = this(Option(new ColorProperty(color)))
  }

  object PolylineGlow {
    implicit val fmt = Json.format[PolylineGlow]

    def apply(color: ColorProperty, glowPower: Number): PolylineGlow = new PolylineGlow(color, glowPower)

    def apply(color: CzmlColor, glowPower: Double): PolylineGlow = new PolylineGlow(color, glowPower)

    def apply(rgba: Rgba, glowPower: Double): PolylineGlow = new PolylineGlow(rgba, glowPower)

    def apply(r: Int, g: Int, b: Int, a: Int, glowPower: Double): PolylineGlow = new PolylineGlow(r, g, b, a, glowPower)

    def apply(rgbaf: Rgbaf, glowPower: Double): PolylineGlow = new PolylineGlow(rgbaf, glowPower)

    def apply(r: Double, g: Double, b: Double, a: Double, glowPower: Double): PolylineGlow = new PolylineGlow(r, g, b, a, glowPower)

    def apply(color: CzmlColor): PolylineGlow = new PolylineGlow(color)

  }

  /**
    * Colors the line with a color and outline.
    *
    * @param color        of the line
    * @param outlineColor color
    * @param outlineWidth with of the outline
    */
  case class PolylineOutline(color: Option[ColorProperty] = None, outlineColor: Option[ColorProperty] = None,
                             outlineWidth: Option[Number] = None) {

    def this(color: ColorProperty, outlineColor: ColorProperty, outlineWidth: Number) = this(Option(color), Option(outlineColor), Option(outlineWidth))

    def this(color: CzmlColor, outlineColor: CzmlColor, outlineWidth: Double) =
      this(Option(new ColorProperty(color)), Option(new ColorProperty(outlineColor)), Option(new Number(outlineWidth)))

    def this(color: Rgba, outlineColor: Rgba, outlineWidth: Double) =
      this(Option(new ColorProperty(color)), Option(new ColorProperty(outlineColor)), Option(new Number(outlineWidth)))

    def this(color: Rgbaf, outlineColor: Rgbaf, outlineWidth: Double) =
      this(Option(new ColorProperty(color)), Option(new ColorProperty(outlineColor)), Option(new Number(outlineWidth)))

    def this(color: CzmlColor) = this(Option(new ColorProperty(color)))

  }

  object PolylineOutline {
    implicit val fmt = Json.format[PolylineOutline]

    def apply(color: ColorProperty, outlineColor: ColorProperty, outlineWidth: Number): PolylineOutline =
      new PolylineOutline(color, outlineColor, outlineWidth)

    def apply(color: CzmlColor, outlineColor: CzmlColor, outlineWidth: Double): PolylineOutline =
      new PolylineOutline(color, outlineColor, outlineWidth)

    def apply(color: Rgba, outlineColor: Rgba, outlineWidth: Double): PolylineOutline =
      new PolylineOutline(color, outlineColor, outlineWidth)

    def apply(color: Rgbaf, outlineColor: Rgbaf, outlineWidth: Double): PolylineOutline =
      new PolylineOutline(color, outlineColor, outlineWidth)

    def apply(color: CzmlColor): PolylineOutline = new PolylineOutline(color)
  }

  /**
    * material used by a line such as a Path or a Polyline
    *
    * @param solidColor      Fills the surface with a solid color, which may be translucent.
    * @param polylineOutline Colors the line with a color and outline.
    * @param polylineGlow    Colors the line with a glowing color.
    */
  case class LineMaterial(solidColor: Option[SolidColor] = None, polylineOutline: Option[PolylineOutline] = None,
                          polylineGlow: Option[PolylineGlow] = None) {

    def this(solidColor: SolidColor, polylineOutline: PolylineOutline, polylineGlow: PolylineGlow) =
      this(Option(solidColor), Option(polylineOutline), Option(polylineGlow))

    def this(solidColor: CzmlColor, polylineOutline: CzmlColor, polylineGlow: CzmlColor) =
      this(Option(new SolidColor(solidColor)), Option(new PolylineOutline(polylineOutline)), Option(new PolylineGlow(polylineGlow)))
  }

  object LineMaterial {
    implicit val fmt = Json.format[LineMaterial]

    def apply(solidColor: SolidColor, polylineOutline: PolylineOutline, polylineGlow: PolylineGlow): LineMaterial =
      new LineMaterial(solidColor, polylineOutline, polylineGlow)

    def apply(solidColor: CzmlColor, polylineOutline: CzmlColor, polylineGlow: CzmlColor): LineMaterial =
      new LineMaterial(solidColor, polylineOutline, polylineGlow)

  }

  /**
    * Indicates what part of a sensor should be displayed.
    *
    * @param portionToDisplay "COMPLETE", "BELOW_ELLIPSOID_HORIZON", "ABOVE_ELLIPSOID_HORIZON"
    * @param reference        A reference property
    */
  case class PortionToDisplay(portionToDisplay: Option[String] = None, reference: Option[String] = None) {
    def this(portionToDisplay: String) = this(Option(portionToDisplay))
  }

  object PortionToDisplay {
    def apply(portionToDisplay: String): PortionToDisplay = new PortionToDisplay(portionToDisplay)

    val theReads = new Reads[PortionToDisplay] {
      def reads(js: JsValue): JsResult[PortionToDisplay] = {
        // try to read a simple string
        val result = JsPath.read[String].reads(js).asOpt match {
          case Some(s) => Some(s)
          case None => (JsPath \ "portionToDisplay").read[String].reads(js).asOpt
        }
        val ref = (JsPath \ "reference").read[String].reads(js).asOpt
        JsSuccess(new PortionToDisplay(result, ref))
      }
    }

    val theWrites = new Writes[PortionToDisplay] {
      def writes(obj: PortionToDisplay) = {
        val thePortion = obj.portionToDisplay match {
          case Some(x) => JsString(x)
          case None => JsNull
        }
        obj.reference match {
          case Some(ref) => Json.obj("portionToDisplay" -> thePortion, "reference" -> JsString(ref))
          case None => thePortion
        }
      }
    }

    implicit val fmt: Format[PortionToDisplay] = Format(theReads, theWrites)
  }

  /**
    * The list of directions defining the pyramid.
    */
  case class Directions(spherical: Option[ArrayBuffer[Double]] = None, unitSpherical: Option[ArrayBuffer[Double]] = None,
                        cartesian: Option[ArrayBuffer[Double]] = None, unitCartesian: Option[ArrayBuffer[Double]] = None)

  object Directions {
    implicit val fmt = Json.format[Directions]
  }

  /**
    * Transformations to apply to a particular node in a 3D model
    *
    * @param scale       Defines an scaling factor which can optionally vary over time.
    * @param translation Defines an translational offset which can optionally vary over time
    * @param rotation
    */
  case class NodeTransformation(scale: Option[CzmlCartesian] = None, translation: Option[CzmlCartesian] = None, rotation: Option[Orientation] = None) {

    def this(sx: Double, sy: Double, sz: Double, tx: Double, ty: Double, tz: Double, rotation: Orientation) =
      this(Option(new CzmlCartesian(sx, sy, sz)), Option(new CzmlCartesian(tx, ty, tz)), Option(rotation))

    def this(scale: Cartesian, translation: Cartesian, rotation: Orientation) =
      this(Option(CzmlCartesian(scale)), Option(CzmlCartesian(translation)), Option(rotation))
  }

  object NodeTransformation {
    implicit val fmt = Json.format[NodeTransformation]

    def apply(sx: Double, sy: Double, sz: Double, tx: Double, ty: Double, tz: Double, rotation: Orientation) =
      new NodeTransformation(sx, sy, sz, tx, ty, tz, rotation)

    def apply(scale: Cartesian, translation: Cartesian, rotation: Orientation): NodeTransformation =
      new NodeTransformation(scale, translation, rotation)
  }

  /**
    * Defines a mapping of node names to node transformations
    *
    * @param nodes the map of node names and node transformations
    */
  case class NodeTransformations(nodes: mutable.ListMap[String, NodeTransformation])

  object NodeTransformations {

    val theReads = new Reads[NodeTransformations] {
      def reads(json: JsValue): JsResult[NodeTransformations] = {
        json match {
          case js: JsObject =>
            val theListMap = mutable.ListMap.empty ++= js.fields.collect {
              case (key, JsObject(value)) => key -> NodeTransformation.fmt.reads(JsObject(value)).getOrElse(new NodeTransformation())
            }
            JsSuccess(new NodeTransformations(theListMap))

          case x =>
            logger.error("could not read NodeTransformations: " + x.toString())
            JsError(s"Could not read NodeTransformations : $x")
        }
      }
    }

    val theWrites = new Writes[NodeTransformations] {
      def writes(nodeTrans: NodeTransformations) = {
        val list = for (n <- nodeTrans.nodes) yield n._1 -> Json.toJson(n._2)
        JsObject(list)
      }
    }

    implicit val fmt: Format[NodeTransformations] = Format(theReads, theWrites)
  }

}
