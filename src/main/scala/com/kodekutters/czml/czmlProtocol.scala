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

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


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
  */
package object czmlProtocol {


  final case class TimeInterval(value: String) {

    import TimeInterval._

    def this(start: String, stop: String) = this(start + "/" + stop)

    def start() = value.split("/").head

    def stop() = value.split("/").last

    def startLocalDateTime() = LocalDateTime.parse(start(), fmter)

    def stopLocalDateTime() = LocalDateTime.parse(stop(), fmter)
  }

  object TimeInterval {

    val fmter = DateTimeFormatter.ISO_DATE_TIME

  }

  //  final case class Duration(days: Int, seconds: Double) {
  //    def this(days: Int, hours: Int, minutes: Int, seconds: Double) = this(days, Duration.toSeconds(hours, minutes, seconds))
  //  }
  //  object Duration {
  //    def toSeconds(hours: Int, minutes: Int, seconds: Double): Double = hours * 3600.0 + minutes * 60.0 + seconds
  //    implicit val fmt = Jsonx.formatCaseClass[Duration]
  //  }

  //  final case class Description(reference: Option[String] = None, string: Option[String] = None)
  //  object Description {
  //    implicit val fmt = Jsonx.formatCaseClass[Description]
  //  }

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

    val aveReads = new Reads[Availability] {
      def reads(js: JsValue): JsResult[Availability] = {
        // try to read it as a String
        val ave = JsPath.read[String].reads(js).asOpt match {
          // fail to read it as a String, so could be an Array[String]
          case None => Right(JsPath.read[Array[String]].reads(js).getOrElse(Array[String]()))
          // is a string
          case Some(t) => Left(t)
        }
        JsSuccess(new Availability(ave))
      }
    }

    val aveWrites = new Writes[Availability] {
      def writes(time: Availability) = {
        time.value match {
          case Left(x) => JsString(x)
          case Right(x) => Json.toJson(x)
        }
      }
    }

    implicit val fmt: Format[Availability] = Format(aveReads, aveWrites)
  }

  /**
    * representing time as a String or a Double
    *
    * @param value the time value
    */
  final case class TimeValue(value: Either[String, Double]) {
    def this(value: String) = this(Left(value))

    def this(value: Double) = this(Right(value))
  }

  object TimeValue {

    def apply(value: String): TimeValue = new TimeValue(value)

    def apply(value: Double): TimeValue = new TimeValue(value)

    val timeValueReads = new Reads[TimeValue] {
      def reads(js: JsValue): JsResult[TimeValue] = {
        // try to read it as a String
        val time = JsPath.read[String].reads(js).asOpt match {
          // fail to read it as a String, time could be a double
          case None => Right(JsPath.read[Double].reads(js).getOrElse(0.0))
          // time is a string
          case Some(t) => Left(t)
        }
        JsSuccess(new TimeValue(time))
      }
    }

    val timeValueWrites = new Writes[TimeValue] {
      def writes(time: TimeValue) = {
        time.value match {
          case Left(x) => JsString(x)
          case Right(x) => JsNumber(x)
        }
      }
    }

    implicit val fmt: Format[TimeValue] = Format(timeValueReads, timeValueWrites)
  }

  /**
    * The base for a property whose value may be determined by
    * interpolating over the provided time-tagged samples.
    */
  trait Interpolatable {
    // Specifies the epoch to use for times specifies as seconds since an epoch.
    def epoch: Option[String]

    // The time of the next sample within this interval, specified as either
    // an ISO 8601 date and time string or as seconds since epoch.
    // This property is used to determine if there is a gap between samples specified in different packets.
    def nextTime: Option[TimeValue]

    // The time of the previous sample within this interval, specified as either
    // an ISO 8601 date and time string or as seconds since epoch.
    // This property is used to determine if there is a gap between samples specified in different packets.
    def previousTime: Option[TimeValue]

    // specifies the algorithm to use to interpolate a value at a different time from the provided data
    def interpolationAlgorithm: Option[String]

    // specifies the degree of the polynomial to use for interpolation
    def interpolationDegree: Option[Int]

    // the type of extrapolation to perform when a value is requested at a time after any available samples.
    def forwardExtrapolationType: Option[String]

    // the amount of time to extrapolate backward before the property becomes undefined.
    // A value of 0 will extrapolate forever.
    def forwardExtrapolationDuration: Option[Double] // Duration todo

    // the type of extrapolation to perform when a value is requested at a time before any available samples.
    def backwardExtrapolationType: Option[String]

    // the amount of time to extrapolate backward before the property becomes undefined.
    // A value of 0 will extrapolate forever.
    def backwardExtrapolationDuration: Option[Double] // Duration todo
  }

  /**
    * an array of west south, east north degrees coordinates for a rectangle
    */
  final case class WsenDegrees(wsenDegrees: Array[Double]) {
    def this(w: Double, s: Double, e: Double, n: Double) = this(Array(w, s, e, n))
  }

  object WsenDegrees {
    implicit val fmt = Json.format[WsenDegrees]

    def apply(w: Double, s: Double, e: Double, n: Double): WsenDegrees = new WsenDegrees(w, s, e, n)
  }

  /**
    * a 3d cartesian coordinate that can have a time component.
    */
  final case class Coordinate(t: Option[TimeValue] = None, x: Double, y: Double, z: Double) {
    def this(x: Double, y: Double, z: Double) = this(None, x, y, z)

    def this(t: String, x: Double, y: Double, z: Double) = this(Option(TimeValue(t)), x, y, z)

    def this(t: Double, x: Double, y: Double, z: Double) = this(Option(TimeValue(t)), x, y, z)

    def this(t: TimeValue, x: Double, y: Double, z: Double) = this(Option(t), x, y, z)
  }

  object Coordinate {
    def apply(x: Double, y: Double, z: Double): Coordinate = new Coordinate(x, y, z)

    def apply(t: String, x: Double, y: Double, z: Double): Coordinate = new Coordinate(t, x, y, z)

    def apply(t: Double, x: Double, y: Double, z: Double): Coordinate = new Coordinate(t, x, y, z)

    def apply(t: TimeValue, x: Double, y: Double, z: Double): Coordinate = new Coordinate(t, x, y, z)
  }

  /**
    * a 2d cartesian coordinate that can have a time component.
    */
  final case class Coordinate2D(t: Option[TimeValue] = None, x: Double, y: Double) {
    def this(x: Double, y: Double) = this(None, x, y)

    def this(t: String, x: Double, y: Double) = this(Option(TimeValue(t)), x, y)

    def this(t: Double, x: Double, y: Double) = this(Option(TimeValue(t)), x, y)

    def this(t: TimeValue, x: Double, y: Double) = this(Option(t), x, y)
  }

  object Coordinate2D {
    def apply(x: Double, y: Double): Coordinate2D = new Coordinate2D(x, y)

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
  final case class Cartesian(coordinates: Seq[Coordinate]) {
    def this(coordinate: Coordinate) = this(Seq(coordinate))

    def this(x: Double, y: Double, z: Double) = this(Seq(Coordinate(x, y, z)))

    def this(t: String, x: Double, y: Double, z: Double) = this(Seq(Coordinate(t, x, y, z)))

    def this(t: Double, x: Double, y: Double, z: Double) = this(Seq(Coordinate(t, x, y, z)))

    def this(t: TimeValue, x: Double, y: Double, z: Double) = this(Seq(Coordinate(t, x, y, z)))
  }

  object Cartesian {

    def apply(x: Double, y: Double, z: Double): Cartesian = new Cartesian(x, y, z)

    def apply(coordinate: Coordinate): Cartesian = new Cartesian(coordinate)

    def apply(t: String, x: Double, y: Double, z: Double): Cartesian = new Cartesian(Coordinate(t, x, y, z))

    def apply(t: Double, x: Double, y: Double, z: Double): Cartesian = new Cartesian(Coordinate(t, x, y, z))

    def apply(t: TimeValue, x: Double, y: Double, z: Double): Cartesian = new Cartesian(Coordinate(t, x, y, z))

    val cartesianReads = new Reads[Cartesian] {
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
          JsSuccess(new Cartesian(Seq(
            ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double] and
              (JsPath \ 2).read[Double]) (Coordinate.apply(None, _, _, _)).reads(js).get)))
        }
      }
    }

    val cartesianWrites = new Writes[Cartesian] {
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

    implicit val fmt: Format[Cartesian] = Format(cartesianReads, cartesianWrites)
  }

  /**
    * A list of 2D Cartesian [X, Y] in viewport coordinates in pixels, where X is pixels to the right and Y is pixels up.
    * If the array has two elements, the pixel offset is constant.
    * If it has three or more elements, they are time-tagged samples arranged as
    * [Time, X, Y, Time, X, Y, Time, X, Y, ...], where _Time_ is an ISO 8601 date and time string
    * or seconds since epoch.
    */
  final case class Cartesian2D(coordinates: Seq[Coordinate2D]) {
    def this(coordinate2D: Coordinate2D) = this(Seq(coordinate2D))

    def this(x: Double, y: Double) = this(Coordinate2D(x, y))

    def this(t: TimeValue, x: Double, y: Double) = this(Coordinate2D(t, x, y))

    def this(t: String, x: Double, y: Double) = this(Seq(Coordinate2D(t, x, y)))

    def this(t: Double, x: Double, y: Double) = this(Seq(Coordinate2D(t, x, y)))

  }

  object Cartesian2D {

    def apply(t: TimeValue, x: Double, y: Double): Cartesian2D = new Cartesian2D(t, x, y)

    def apply(t: String, x: Double, y: Double): Cartesian2D = new Cartesian2D(t, x, y)

    def apply(t: Double, x: Double, y: Double): Cartesian2D = new Cartesian2D(t, x, y)

    def apply(x: Double, y: Double): Cartesian2D = new Cartesian2D(x, y)

    def apply(coordinate: Coordinate2D): Cartesian2D = new Cartesian2D(coordinate)

    val cartesianReads = new Reads[Cartesian2D] {
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
          JsSuccess(new Cartesian2D(Seq(
            ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double]) (Coordinate2D.apply(None, _, _)).reads(js).get)))
        }
      }
    }

    val cartesianWrites = new Writes[Cartesian2D] {
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

    implicit val fmt: Format[Cartesian2D] = Format(cartesianReads, cartesianWrites)
  }

  /**
    * A timed geodetic (time,long,lat,alt) coordinate. The values can represent either degrees or radians
    */
  final case class LngLatAltT(t: Option[TimeValue] = None, lng: Double, lat: Double, alt: Double) {

    def this(lng: Double, lat: Double, alt: Double) = this(None, lng, lat, alt)

    def this(t: TimeValue, lng: Double, lat: Double, alt: Double) = this(Option(t), lng, lat, alt)

    def this(t: String, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)

    def this(t: Double, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)
  }

  object LngLatAltT {

    def apply(lng: Double, lat: Double, alt: Double): LngLatAltT = new LngLatAltT(lng, lat, alt)

    def apply(t: TimeValue, lng: Double, lat: Double, alt: Double): LngLatAltT = new LngLatAltT(Option(t), lng, lat, alt)

    def apply(t: String, lng: Double, lat: Double, alt: Double): LngLatAltT = new LngLatAltT(TimeValue(t), lng, lat, alt)

    def apply(t: Double, lng: Double, lat: Double, alt: Double): LngLatAltT = new LngLatAltT(TimeValue(t), lng, lat, alt)

  }

  /**
    * A list of geodetic, WGS84 positions using longitude, latitude, and height components.
    * The positions represented as a WGS 84 Cartographic [Longitude, Latitude, Height]
    * where longitude and latitude are in degrees and height is in meters.
    * If the array has three elements, the position is constant.
    * If it has four or more elements, they are time-tagged samples arranged
    * as [Time, Longitude, Latitude, Height, Time, Longitude, Latitude, Height, ...],
    * where Time is an ISO 8601 date and time string or seconds since "epoch".
    * Note: the longitudes and latitudes can be in decimal degrees or radians
    */
  final case class Cartographic(coordinates: Seq[LngLatAltT]) {

    def this(lngLatAltT: LngLatAltT) = this(Seq(lngLatAltT))

    def this(lng: Double, lat: Double, alt: Double) = this(LngLatAltT(lng, lat, alt))

    def this(t: TimeValue, lng: Double, lat: Double, alt: Double) = this(LngLatAltT(t, lng, lat, alt))

    def this(t: String, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)

    def this(t: Double, lng: Double, lat: Double, alt: Double) = this(TimeValue(t), lng, lat, alt)
  }

  object Cartographic {

    def apply(lngLatAltT: LngLatAltT): Cartographic = new Cartographic(Seq(lngLatAltT))

    def apply(lng: Double, lat: Double, alt: Double): Cartographic = new Cartographic(LngLatAltT(lng, lat, alt))

    def apply(t: TimeValue, lng: Double, lat: Double, alt: Double): Cartographic = new Cartographic(LngLatAltT(t, lng, lat, alt))

    def apply(t: String, lng: Double, lat: Double, alt: Double): Cartographic = new Cartographic(TimeValue(t), lng, lat, alt)

    def apply(t: Double, lng: Double, lat: Double, alt: Double): Cartographic = new Cartographic(TimeValue(t), lng, lat, alt)

    val cartographicReads = new Reads[Cartographic] {
      def reads(js: JsValue): JsResult[Cartographic] = {
        val jsList = js.as[JsArray].value
        // have a list of timed coordinates, multiple of 4 elements
        if (jsList.length >= 4 && (jsList.length % 4) == 0) {
          JsSuccess(new Cartographic(
            (for (i <- jsList.indices by 4) yield
              ((JsPath \ i).readNullable[TimeValue] and
                (JsPath \ (i + 1)).read[Double] and (JsPath \ (i + 2)).read[Double] and
                (JsPath \ (i + 3)).read[Double]) (LngLatAltT.apply(_, _, _, _)).reads(js).asOpt).flatten))
        }
        // have a single coordinate [lng,lat,alt]
        else {
          JsSuccess(new Cartographic(Seq(
            ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double] and
              (JsPath \ 2).read[Double]) (LngLatAltT.apply(None, _, _, _)).reads(js).get)))
        }
      }
    }

    val cartographicWrites = new Writes[Cartographic] {
      def writes(cart: Cartographic) = {
        val coordList = for (coord <- cart.coordinates) yield {
          coord.t match {
            case Some(time) => List(TimeValue.fmt.writes(time), JsNumber(coord.lng), JsNumber(coord.lat), JsNumber(coord.alt))
            case None => List(JsNumber(coord.lng), JsNumber(coord.lat), JsNumber(coord.alt))
          }
        }
        JsArray(coordList.flatten)
      }
    }

    implicit val fmt: Format[Cartographic] = Format(cartographicReads, cartographicWrites)
  }

  /**
    * A timed velocity element.
    */
  final case class Velocity(t: Option[TimeValue] = None, x: Double, y: Double, z: Double, vx: Double, vy: Double, vz: Double) {
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
  final case class CartesianVelocity(velocities: Seq[Velocity]) {
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


    val velocityReads = new Reads[CartesianVelocity] {
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
          JsSuccess(new CartesianVelocity(Seq(
            ((JsPath \ 0).read[Double] and (JsPath \ 1).read[Double] and
              (JsPath \ 2).read[Double] and (JsPath \ 3).read[Double] and
              (JsPath \ 4).read[Double] and (JsPath \ 5).read[Double]
              ) (Velocity.apply(None, _, _, _, _, _, _)).reads(js).get)))
        }
      }
    }

    val velocityWrites = new Writes[CartesianVelocity] {
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

    implicit val fmt: Format[CartesianVelocity] = Format(velocityReads, velocityWrites)
  }

  /**
    * describes a boolean interval for use in Show
    */
  final case class BooleanInterval(interval: Option[String] = None, boolean: Option[Boolean]) {
    def this(interval: String, boolean: Boolean) = this(Option(interval), Option(boolean))
  }

  object BooleanInterval {
    implicit val fmt = Json.format[BooleanInterval]

    def apply(interval: String, boolean: Boolean): BooleanInterval = new BooleanInterval(interval, boolean)
  }

  /**
    * determines whether an object is shown or not
    *
    * @param value could be a simple boolean or an array of BooleanInterval
    */
  final case class Show(value: Either[Boolean, Array[BooleanInterval]]) {
    def this(value: Boolean) = this(Left(value))

    def this(value: Array[BooleanInterval]) = this(Right(value))

    def this(value: BooleanInterval) = this(Right(Array(value)))

    def this(interval: String, boolean: Boolean) = this(Right(Array(new BooleanInterval(interval, boolean))))
  }

  object Show {

    def apply(value: Boolean): Show = new Show(value)

    def apply(interval: String, boolean: Boolean): Show = new Show(interval, boolean)

    val showReads = new Reads[Show] {
      def reads(js: JsValue): JsResult[Show] = {
        // try to read a simple boolean
        val result = JsPath.read[Boolean].reads(js).asOpt match {
          case None => Right(JsPath.read[Array[BooleanInterval]].reads(js).getOrElse(Array[BooleanInterval]()))
          case Some(b) => Left(b)
        }
        JsSuccess(new Show(result))
      }
    }

    val showWrites = new Writes[Show] {
      def writes(shw: Show) = {
        shw.value match {
          case Left(x) => JsBoolean(x)
          case Right(x) => Json.toJson(x)
        }
      }
    }

    implicit val fmt: Format[Show] = Format(showReads, showWrites)
  }

  /**
    * A color specified as an array of color components [Red, Green, Blue, Alpha]
    * where each component is in the range 0-255.
    */
  final case class Rgba(t: Option[TimeValue] = None, r: Int, g: Int, b: Int, a: Int) {
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
  final case class RgbaList(values: Seq[Rgba]) {
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

    val rgbaReads = new Reads[RgbaList] {
      def reads(js: JsValue): JsResult[RgbaList] = {
        val jsList = js.as[JsArray].value
        // have a list of timed values
        if (jsList.length >= 5 && (jsList.length % 5) == 0) {
          JsSuccess(new RgbaList(
            (for (i <- jsList.indices by 5) yield ((JsPath \ i).readNullable[TimeValue] and
              (JsPath \ (i + 1)).read[Int] and (JsPath \ (i + 2)).read[Int] and
              (JsPath \ (i + 3)).read[Int] and (JsPath \ (i + 4)).read[Int]
              ) (Rgba.apply(_, _, _, _, _)).reads(js).asOpt).flatten))
        }
        // have a single color Rgba
        else {
          JsSuccess(new RgbaList(Seq(
            ((JsPath \ 0).read[Int] and (JsPath \ 1).read[Int] and
              (JsPath \ 2).read[Int] and (JsPath \ 3).read[Int]) (Rgba.apply(None, _, _, _, _)).reads(js).get)))
        }
      }
    }

    val rgbaWrites = new Writes[RgbaList] {
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

    implicit val fmt: Format[RgbaList] = Format(rgbaReads, rgbaWrites)
  }

  /**
    * The color specified as an array of color components [Red, Green, Blue, Alpha]
    * where each component is a float in the range 0.0-1.0.
    */
  final case class Rgbaf(t: Option[TimeValue] = None, r: Float, g: Float, b: Float, a: Float) {
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
  final case class RgbafList(values: Seq[Rgbaf]) {
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


    val rgbafReads = new Reads[RgbafList] {
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
          JsSuccess(new RgbafList(Seq(
            ((JsPath \ 0).read[Float] and (JsPath \ 1).read[Float] and
              (JsPath \ 2).read[Float] and (JsPath \ 3).read[Float]) (Rgbaf.apply(None, _, _, _, _)).reads(js).get)))
        }
      }
    }

    val rgbafWrites = new Writes[RgbafList] {
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

    implicit val fmt: Format[RgbafList] = Format(rgbafReads, rgbafWrites)
  }

  /**
    * a time and double value pair
    *
    * @param t the time component
    * @param v the value component
    */
  final case class TimedDouble(t: Option[String] = None, v: Double) {
    def this(t: String, v: Double) = this(Option(t), v)
  }

  object TimedDouble {
    implicit val fmt = Json.format[TimedDouble]

    def apply(t: String, v: Double): TimedDouble = new TimedDouble(t, v)
  }

  /**
    * represents a sequence of TimedDouble objects or a single instance of TimedDouble.
    *
    * @param values a sequence of TimedDouble objects.
    */
  final case class TimedNumbers(values: Seq[TimedDouble]) {

    def this(t: String, v: Double) = this(Seq(new TimedDouble(t, v)))
  }

  object TimedNumbers {

    def apply(t: String, v: Double): TimedNumbers = new TimedNumbers(t, v)

    val timedValuesReads = new Reads[TimedNumbers] {
      def reads(js: JsValue): JsResult[TimedNumbers] = {

        // could be an array or a single number
        js.asOpt[JsNumber] match {
          // have an array
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
              JsSuccess(new TimedNumbers(Seq(
                (JsPath \ 0).read[Double].map(TimedDouble.apply(None, _)).reads(js).get)))
            }

          // have a single value with no time component
          case Some(n) =>
            JsSuccess(new TimedNumbers(Seq(JsPath.read[Double].map(TimedDouble.apply(None, _)).reads(js).get)))
        }
      }
    }

    val timedValuesWrites = new Writes[TimedNumbers] {
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

    implicit val fmt: Format[TimedNumbers] = Format(timedValuesReads, timedValuesWrites)
  }

  /**
    * The eye offset of the billboard or label, which is the offset in eye coordinates at which to place
    * the billboard or label relative to the position property. Eye coordinates are a left-handed coordinate system
    * where the X-axis points toward the viewer's right, the Y-axis points up, and the Z-axis points into the screen.
    */
  final case class EyeOffset(cartesian: Option[Cartesian] = None, reference: Option[String] = None, epoch: Option[String] = None,
                             nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                             interpolationAlgorithm: Option[String] = None,
                             interpolationDegree: Option[Int] = None,
                             forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                             backwardExtrapolationType: Option[String] = None,
                             backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

    def this(cartesian: Cartesian) = this(Option(cartesian))

    def this(x: Double, y: Double, z: Double) = this(Cartesian(x, y, z))
  }

  object EyeOffset {
    implicit val fmt = Json.format[EyeOffset]

    def apply(cartesian: Cartesian): EyeOffset = new EyeOffset(cartesian)

    def apply(x: Double, y: Double, z: Double): EyeOffset = new EyeOffset(x, y, z)
  }

  /**
    * The horizontal origin of the billboard. It controls whether the billboard image
    * is left-, center-, or right-aligned with the position.
    *
    * @param horizontalOrigin The horizontal origin. Valid values are "LEFT", "CENTER", and "RIGHT".
    * @param reference        A reference property.
    */
  final case class HorizontalOrigin(horizontalOrigin: Option[String] = None, reference: Option[String] = None) {
    def this(horizontalOrigin: String) = this(Option(horizontalOrigin))

    def this(horizontalOrigin: String, reference: String) = this(Option(horizontalOrigin), Option(reference))
  }

  object HorizontalOrigin {
    def apply(horizontalOrigin: String): HorizontalOrigin = new HorizontalOrigin(horizontalOrigin)

    def apply(horizontalOrigin: String, reference: String): HorizontalOrigin = new HorizontalOrigin(horizontalOrigin, reference)

    val hReads = new Reads[HorizontalOrigin] {
      def reads(js: JsValue): JsResult[HorizontalOrigin] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new HorizontalOrigin((JsPath \ "horizontalOrigin").read[String].reads(js).asOpt,
              (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(hz) => JsSuccess(new HorizontalOrigin(Some(hz)))
        }
      }
    }

    val hWrites = new Writes[HorizontalOrigin] {
      def writes(obj: HorizontalOrigin) = {
        obj.reference match {
          case Some(ref) => Json.obj("horizontalOrigin" -> JsString(obj.horizontalOrigin.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.horizontalOrigin.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[HorizontalOrigin] = Format(hReads, hWrites)
  }

  /**
    * The vertical origin of the billboard.
    * It controls whether the billboard image is bottom-, center-, or top-aligned with the position.
    *
    * @param verticalOrigin verticalOrigin = ["BOTTOM", "CENTER", "TOP"]
    * @param reference      A reference property
    */
  final case class VerticalOrigin(verticalOrigin: Option[String] = None, reference: Option[String] = None) {
    def this(verticalOrigin: String) = this(Option(verticalOrigin))

    def this(verticalOrigin: String, reference: String) = this(Option(verticalOrigin), Option(reference))
  }

  object VerticalOrigin {

    def apply(verticalOrigin: String): VerticalOrigin = new VerticalOrigin(verticalOrigin)

    def apply(verticalOrigin: String, reference: String): VerticalOrigin = new VerticalOrigin(verticalOrigin, reference)

    val vReads = new Reads[VerticalOrigin] {
      def reads(js: JsValue): JsResult[VerticalOrigin] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new VerticalOrigin((JsPath \ "verticalOrigin").read[String].reads(js).asOpt,
              (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(v) => JsSuccess(new VerticalOrigin(Some(v)))
        }
      }
    }

    val vWrites = new Writes[VerticalOrigin] {
      def writes(obj: VerticalOrigin) = {
        obj.reference match {
          case Some(ref) => Json.obj("verticalOrigin" -> JsString(obj.verticalOrigin.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.verticalOrigin.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[VerticalOrigin] = Format(vReads, vWrites)
  }

  /**
    * The image displayed on the billboard, expressed as a URL. For broadest client compatibility,
    * the URL should be accessible via Cross-Origin Resource Sharing (CORS). The URL may also be a data URI.
    *
    * @param uri       A URI value.  The URI can optionally vary with time.  todo ?
    * @param reference A reference property.
    */
  final case class ImageUri(uri: Option[String] = None, reference: Option[String] = None) {
    def this(uri: String) = this(Option(uri))

    def this(uri: String, reference: String) = this(Option(uri), Option(reference))
  }

  object ImageUri {

    def apply(uri: String): ImageUri = new ImageUri(uri)

    def apply(uri: String, reference: String): ImageUri = new ImageUri(uri, reference)


    val uriReads = new Reads[ImageUri] {
      def reads(js: JsValue): JsResult[ImageUri] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new ImageUri((JsPath \ "uri").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(ur) => JsSuccess(new ImageUri(Some(ur)))
        }
      }
    }

    val uriWrites = new Writes[ImageUri] {
      def writes(obj: ImageUri) = {
        obj.reference match {
          case Some(ref) => Json.obj("uri" -> JsString(obj.uri.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.uri.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[ImageUri] = Format(uriReads, uriWrites)
  }

  /**
    * The offset, in viewport pixels, of the billboard origin from the position. A pixel offset is
    * the number of pixels up and to the right to place the billboard, relative to the position.
    */
  final case class PixelOffset(cartesian2: Option[Cartesian2D] = None, reference: Option[String] = None, epoch: Option[String] = None,
                               nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                               interpolationAlgorithm: Option[String] = None,
                               interpolationDegree: Option[Int] = None,
                               forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                               backwardExtrapolationType: Option[String] = None,
                               backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

    def this(cartesian2: Cartesian2D) = this(Option(cartesian2))

    def this(x: Double, y: Double) = this(new Cartesian2D(x, y))

    def this(t: TimeValue, x: Double, y: Double) = this(new Cartesian2D(t, x, y))

    def this(t: String, x: Double, y: Double) = this(TimeValue(t), x, y)

    def this(t: Double, x: Double, y: Double) = this(TimeValue(t), x, y)
  }

  object PixelOffset {
    implicit val fmt = Json.format[PixelOffset]

    def apply(cartesian2: Cartesian2D): PixelOffset = new PixelOffset(cartesian2)

    def apply(x: Double, y: Double): PixelOffset = new PixelOffset(x, y)

    def apply(t: TimeValue, x: Double, y: Double): PixelOffset = new PixelOffset(t, x, y)

    def apply(t: String, x: Double, y: Double): PixelOffset = new PixelOffset(TimeValue(t), x, y)

    def apply(t: Double, x: Double, y: Double): PixelOffset = new PixelOffset(TimeValue(t), x, y)
  }

  /**
    * A generic number that can be timed and is interpolatable.
    *
    * Note: if epoch is defined "number" becomes an array of doubles.
    * if epoch is not defined, then "number" becomes either an array of timed values (see TimedNumbers) or
    * a single double value.
    */
  final case class CzmlNumber(number: Option[Any] = None, interval: Option[String] = None,
                              reference: Option[String] = None, epoch: Option[String] = None,
                              nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                              interpolationAlgorithm: Option[String] = None,
                              interpolationDegree: Option[Int] = None,
                              forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                              backwardExtrapolationType: Option[String] = None,
                              backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

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

    val czmlNumberReads = new Reads[CzmlNumber] {
      def reads(js: JsValue): JsResult[CzmlNumber] = {
        // read the full set of fields
        def doFullRead(): JsResult[CzmlNumber] = {
          val epoch = (JsPath \ "epoch").read[String].reads(js).asOpt
          val number = epoch match {
            // no epoch -> number field could be a list of TimedNumbers
            case None => (JsPath \ "number").read[TimedNumbers].reads(js).asOpt
            // with epoch -> number field could be an array of doubles
            case Some(epok) =>
              (js \ "number").asOpt[Array[Double]] match {
                case None => (js \ "number").asOpt[Double]
                case Some(arr) => Some(arr)
              }
          }
          val interv = (JsPath \ "interval").read[String].reads(js).asOpt
          val ref = (JsPath \ "reference").read[String].reads(js).asOpt
          val nxt = (JsPath \ "nextTime").read[TimeValue].reads(js).asOpt
          val prev = (JsPath \ "previousTime").read[TimeValue].reads(js).asOpt
          val algo = (JsPath \ "interpolationAlgorithm").read[String].reads(js).asOpt
          val deg = (JsPath \ "interpolationDegree").read[Int].reads(js).asOpt
          val frwExT = (JsPath \ "forwardExtrapolationType").read[String].reads(js).asOpt
          val frwExD = (JsPath \ "forwardExtrapolationDuration").read[Double].reads(js).asOpt
          val bkwExT = (JsPath \ "backwardExtrapolationType").read[String].reads(js).asOpt
          val bkwExD = (JsPath \ "backwardExtrapolationDuration").read[Double].reads(js).asOpt

          JsSuccess(new CzmlNumber(number, interv, ref, epoch, nxt, prev, algo, deg, frwExT, frwExD, bkwExT, bkwExD))
        }

        // check that we could have a simple double
        JsPath.read[Double].reads(js).asOpt match {
          case Some(n) => JsSuccess(new CzmlNumber(Some(n)))
          case None => doFullRead()
        }
      }
    }

    val czmlNumberWrites = new Writes[CzmlNumber] {
      def writes(czmlN: CzmlNumber) = {

        val theList = ListBuffer(
          czmlN.reference.map("reference" -> JsString(_)),
          czmlN.epoch.map("epoch" -> JsString(_)),
          czmlN.interval.map("interval" -> JsString(_)),
          czmlN.nextTime.map("nextTime" -> TimeValue.fmt.writes(_)),
          czmlN.previousTime.map("previousTime" -> TimeValue.fmt.writes(_)),
          czmlN.interpolationAlgorithm.map("interpolationAlgorithm" -> JsString(_)),
          czmlN.interpolationDegree.map("interpolationDegree" -> JsNumber(_)),
          czmlN.forwardExtrapolationType.map("forwardExtrapolationType" -> JsString(_)),
          czmlN.forwardExtrapolationDuration.map("forwardExtrapolationDuration" -> JsNumber(_)),
          czmlN.backwardExtrapolationType.map("backwardExtrapolationType" -> JsString(_)),
          czmlN.backwardExtrapolationDuration.map("backwardExtrapolationDuration" -> JsNumber(_)))

        def doFullWrite() = {
          // number could be written as a JsArray or a JsNumber depending on epoch
          val number = czmlN.epoch match {
            // no epoch the number field could be a list of TimedNumbers (which itself can be a single double)
            case None => czmlN.number.map(x =>
              if (x.isInstanceOf[TimedNumbers]) TimedNumbers.fmt.writes(x.asInstanceOf[TimedNumbers]))

            // with an epoch the number field could be an array of doubles or a single double
            case Some(epok) => czmlN.number.map(x => x match {
              case v if v.isInstanceOf[Array[Double]] => Json.toJson(v.asInstanceOf[Array[Double]])
              case v if v.isInstanceOf[Double] => JsNumber(v.asInstanceOf[Double])
            })
          }

          // add the number to theList
          number match {
            case Some(x) if x.isInstanceOf[JsNumber] => theList += Option("number" -> x.asInstanceOf[JsNumber])
            case Some(x) if x.isInstanceOf[JsArray] => theList += Option("number" -> x.asInstanceOf[JsArray])
          }

          JsObject(theList.flatten)
        }

        // write a simple number or a number with the extra fields
        theList.flatten.isEmpty match {
          // have a simple number
          case true =>
            czmlN.number match {
              case Some(x) if x.isInstanceOf[TimedNumbers] => JsNumber(x.asInstanceOf[TimedNumbers].values.head.v)
              case Some(x) if x.isInstanceOf[Double] => Json.obj("number" -> JsNumber(x.asInstanceOf[Double]))
              case _ => JsNull
            }
          case false => doFullWrite()
        }

      }
    }

    implicit val fmt: Format[CzmlNumber] = Format(czmlNumberReads, czmlNumberWrites)
  }

  /**
    * A number property consisting of a possible list of CzmlNumber
    *
    * @param values the list of CzmlNumbers
    */
  final case class Number(values: Option[Array[CzmlNumber]]) {
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
            else Json.toJson(list.asInstanceOf[Array[CzmlNumber]])

        }
      }
    }

    implicit val fmt: Format[Number] = Format(theReads, theWrites)
  }


  /**
    * Defines a color property. The color can optionally vary over time.
    *
    * @param rgba     A color specified as an array of color components [Red, Green, Blue, Alpha]
    *                 where each component is in the range 0-255. If the array has four elements,
    *                 the color is constant. If it has five or more elements,
    *                 they are time-tagged samples arranged as
    *                 [Time, Red, Green, Blue, Alpha, Time, Red, Green, Blue, Alpha, ...],
    *                 where Time is an ISO 8601 date and time string or seconds since epoch.
    * @param rgbaf    The color specified as an array of color components [Red, Green, Blue, Alpha]
    *                 where each component is in the range 0.0-1.0. If the array has four elements,
    *                 the color is constant. If it has five or more elements, they are time-tagged samples
    *                 arranged as [Time, Red, Green, Blue, Alpha, Time, Red, Green, Blue, Alpha, ...],
    *                 where Time is an ISO 8601 date and time string or seconds since epoch.
    * @param interval the interval property
    */
  final case class CzmlColor(rgba: Option[RgbaList] = None, rgbaf: Option[RgbafList] = None,
                             interval: Option[String] = None,
                             reference: Option[String] = None, epoch: Option[String] = None,
                             nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                             interpolationAlgorithm: Option[String] = None, interpolationDegree: Option[Int] = None,
                             forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                             backwardExtrapolationType: Option[String] = None,
                             backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

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
    implicit val fmt = Json.format[CzmlColor]

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
  }

  /**
    * A color property represented as an array of color objects
    *
    * @param values the array of Colors
    */
  final case class ColorProperty(values: Option[Array[CzmlColor]]) {
    def this(values: Array[CzmlColor]) = this(Option(values))

    def this(color: CzmlColor) = this(Option(Array(color)))

    def this(rgba: Rgba) = this(Option(Array(new CzmlColor(new RgbaList(rgba)))))

    def this(r: Int, g: Int, b: Int, a: Int) = this(Option(Array(new CzmlColor(new RgbaList(new Rgba(r, g, b, a))))))

    def this(rgbaf: Rgbaf) = this(Option(Array(new CzmlColor(new RgbafList(rgbaf)))))

    def this(r: Float, g: Float, b: Float, a: Float) = this(Option(Array(new CzmlColor(new RgbafList(new Rgbaf(r, g, b, a))))))

    def this(r: Double, g: Double, b: Double, a: Double) = this(Option(Array(new CzmlColor(new RgbafList(new Rgbaf(r, g, b, a))))))
  }

  object ColorProperty {

    def apply(values: Array[CzmlColor]): ColorProperty = new ColorProperty(values)

    def apply(color: CzmlColor): ColorProperty = new ColorProperty(color)

    def apply(rgba: Rgba): ColorProperty = new ColorProperty(rgba)

    def apply(r: Int, g: Int, b: Int, a: Int): ColorProperty = new ColorProperty(r, g, b, a)

    def apply(rgbaf: Rgbaf): ColorProperty = new ColorProperty(rgbaf)

    def apply(r: Float, g: Float, b: Float, a: Float): ColorProperty = new ColorProperty(r, g, b, a)

    def apply(r: Double, g: Double, b: Double, a: Double): ColorProperty = new ColorProperty(r, g, b, a)


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
            if (list.length == 1) CzmlColor.fmt.writes(list.head) else Json.toJson(list.asInstanceOf[Array[CzmlColor]])
        }
      }
    }

    implicit val fmt: Format[ColorProperty] = Format(theReads, theWrites)
  }

  /**
    * The aligned axis is the unit vector, in world coordinates, that the billboard up vector points towards.
    * The default is the zero vector, which means the billboard is aligned to the screen up vector.
    */
  final case class AlignedAxis(cartesian: Option[Cartesian] = None, reference: Option[String] = None, epoch: Option[String] = None,
                               nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                               interpolationAlgorithm: Option[String] = None,
                               interpolationDegree: Option[Int] = None,
                               forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                               backwardExtrapolationType: Option[String] = None,
                               backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

    def this(cartesian: Cartesian) = this(Option(cartesian))

    def this(x: Double, y: Double, z: Double) = this(Option(new Cartesian(x, y, z)))
  }

  object AlignedAxis {
    implicit val fmt = Json.format[AlignedAxis]

    def apply(cartesian: Cartesian): AlignedAxis = new AlignedAxis(cartesian)

    def apply(x: Double, y: Double, z: Double): AlignedAxis = new AlignedAxis(x, y, z)
  }

  /**
    * A billboard, or viewport-aligned image. The billboard is positioned in the scene by the position property.
    * A billboard is sometimes called a marker.
    */
  final case class Billboard(color: Option[ColorProperty] = None, eyeOffset: Option[EyeOffset] = None,
                             horizontalOrigin: Option[HorizontalOrigin] = None, image: Option[ImageUri] = None,
                             pixelOffset: Option[PixelOffset] = None, scale: Option[Number] = None,
                             rotation: Option[Number] = None, alignedAxis: Option[AlignedAxis] = None,
                             show: Option[Show] = None, verticalOrigin: Option[VerticalOrigin] = None) extends CzmlProperty {

    def this(color: ColorProperty) = this(color = Option(color), show = Option(Show(true)))

    def this(color: CzmlColor) = this(color = Option(ColorProperty(color)), show = Option(Show(true)))

    def this(rgba: Rgba) = this(color = Option(ColorProperty(rgba)), show = Option(Show(true)))

    def this(r: Int, g: Int, b: Int, a: Int) = this(Option(ColorProperty(r, g, b, a)), show = Option(Show(true)))

    def this(rgbaf: Rgbaf) = this(color = Option(ColorProperty(rgbaf)), show = Option(Show(true)))

    def this(r: Float, g: Float, b: Float, a: Float) = this(Option(ColorProperty(r, g, b, a)), show = Option(Show(true)))

    def this(r: Double, g: Double, b: Double, a: Double) = this(Option(ColorProperty(r, g, b, a)), show = Option(Show(true)))

    def this(image: ImageUri) = this(image = Option(image), show = Option(Show(true)))

    def this(uri: String) = this(image = Option(ImageUri(uri)), show = Option(Show(true)))

    def this(uri: String, scale: Double) = this(image = Option(ImageUri(uri)), scale = Option(Number(scale)), show = Option(Show(true)))

    def this(image: ImageUri, scale: Double) = this(image = Option(image), scale = Option(Number(scale)), show = Option(Show(true)))

    def this(image: ImageUri, scale: Double, rotation: Double) = this(image = Option(image), scale = Option(Number(scale)),
      rotation = Option(Number(rotation)), show = Option(Show(true)))
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

  }

  object Orientation {
    implicit val fmt = Json.format[Orientation]

    def apply(axes: String): Orientation = new Orientation(axes)
  }

  /**
    * A point, or viewport-aligned circle. The point is positioned in the scene by the position property.
    */
  final case class Point(color: Option[ColorProperty] = None, outlineColor: Option[ColorProperty] = None,
                         outlineWidth: Option[Number] = None, pixelSize: Option[Number] = None,
                         show: Option[Show] = None) extends CzmlProperty {

    def this(color: ColorProperty, outlineColor: ColorProperty, outlineWidth: Number, pixelSize: Number) =
      this(Option(color), Option(outlineColor), Option(outlineWidth), Option(pixelSize), Option(new Show(true)))

    def this(color: CzmlColor, outlineColor: CzmlColor, outlineWidth: Double, pixelSize: Double) =
      this(Option(new ColorProperty(color)), Option(new ColorProperty(outlineColor)),
        Option(new Number(outlineWidth)), Option(new Number(pixelSize)), Option(new Show(true)))
  }

  object Point {
    implicit val fmt = Json.format[Point]

    def apply(color: ColorProperty, outlineColor: ColorProperty, outlineWidth: Number, pixelSize: Number): Point =
      new Point(color, outlineColor, outlineWidth, pixelSize)

    def apply(color: CzmlColor, outlineColor: CzmlColor, outlineWidth: Double, pixelSize: Double): Point =
      new Point(color, outlineColor, outlineWidth, pixelSize)

  }

  /**
    * a font
    */
  final case class Font(font: Option[String] = None, reference: Option[String] = None) {
    def this(font: String) = this(Option(font))

    def this(font: String, reference: String) = this(Option(font), Option(reference))
  }

  object Font {

    def apply(font: String): Font = new Font(font)

    def apply(font: String, reference: String): Font = new Font(font, reference)

    val fReads = new Reads[Font] {
      def reads(js: JsValue): JsResult[Font] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new Font((JsPath \ "font").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(f) => JsSuccess(new Font(Some(f)))
        }
      }
    }

    val fWrites = new Writes[Font] {
      def writes(obj: Font) = {
        obj.reference match {
          case Some(ref) => Json.obj("font" -> JsString(obj.font.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.font.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[Font] = Format(fReads, fWrites)
  }

  /**
    * describes a string value with an interval for use in Text
    */
  final case class StringInterval(interval: Option[String] = None, string: Option[String]) {
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
  final case class Text(string: Either[String, Array[StringInterval]], reference: Option[String] = None) {

    def this(string: String) = this(Left(string))

    def this(string: Array[StringInterval]) = this(Right(string))

    def this(string: StringInterval) = this(Right(Array(string)))

    def this(string: String, reference: String) = this(Right(Array(new StringInterval(string, reference))))

    def this(string: String, interval: String, reference: String) =
      this(Right(Array(new StringInterval(interval, string))), Option(reference))

  }

  object Text {

    def apply(string: String): Text = new Text(string)

    def apply(string: String, reference: String): Text = new Text(string, reference)

    val textReads = new Reads[Text] {
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

    val textWrites = new Writes[Text] {
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

    implicit val fmt: Format[Text] = Format(textReads, textWrites)
  }

  /**
    * The style of a label.
    *
    * @param labelStyle "FILL", "OUTLINE", and "FILL_AND_OUTLINE"
    * @param reference  A reference property
    */
  final case class Style(labelStyle: Option[String] = None, reference: Option[String] = None) {
    def this(labelStyle: String) = this(Option(labelStyle))

    def this(labelStyle: String, reference: String) = this(Option(labelStyle), Option(reference))
  }

  object Style {

    def apply(labelStyle: String): Style = new Style(labelStyle)

    def apply(labelStyle: String, reference: String): Style = new Style(labelStyle, reference)


    val sReads = new Reads[Style] {
      def reads(js: JsValue): JsResult[Style] = {
        // try to read a simple String
        JsPath.read[String].reads(js).asOpt match {
          case None => JsSuccess(
            new Style((JsPath \ "labelStyle").read[String].reads(js).asOpt, (JsPath \ "reference").read[String].reads(js).asOpt))

          case Some(s) => JsSuccess(new Style(Some(s)))
        }
      }
    }

    val sWrites = new Writes[Style] {
      def writes(obj: Style) = {
        obj.reference match {
          case Some(ref) => Json.obj("labelStyle" -> JsString(obj.labelStyle.getOrElse("")), "reference" -> JsString(ref))
          case None => JsString(obj.labelStyle.getOrElse(""))
        }
      }
    }

    implicit val fmt: Format[Style] = Format(sReads, sWrites)
  }

  /**
    * A string of text. The label is positioned in the scene by the position property.
    */
  final case class Label(eyeOffset: Option[EyeOffset] = None, fillColor: Option[ColorProperty] = None, font: Option[Font] = None,
                         horizontalOrigin: Option[HorizontalOrigin] = None, outlineColor: Option[ColorProperty] = None,
                         outlineWidth: Option[Number] = None, pixelOffset: Option[PixelOffset] = None,
                         scale: Option[Number] = None, show: Option[Show] = None, style: Option[Style] = None,
                         text: Option[Text] = None, verticalOrigin: Option[VerticalOrigin] = None) extends CzmlProperty {

    def this(text: String) = this(text = Option(Text(text)), show = Option(Show(true)))

    def this(text: String, font: String) = this(text = Option(Text(text)), font = Option(Font(font)), show = Option(Show(true)))
  }

  object Label {
    implicit val fmt = Json.format[Label]

    def apply(text: String): Label = new Label(text)

    def apply(text: String, font: String): Label = new Label(text, font)

  }

  /**
    * The position of the object in the world. The position has no direct visual representation,
    * but it is used to locate billboards, labels, and other primitives attached to the object.
    */
  final case class CzmlPosition(referenceFrame: Option[String] = None, cartesian: Option[Cartesian] = None,
                                cartographicRadians: Option[Cartographic] = None,
                                cartographicDegrees: Option[Cartographic] = None,
                                cartesianVelocity: Option[CartesianVelocity] = None,
                                interval: Option[String] = None,
                                reference: Option[String] = None, epoch: Option[String] = None,
                                nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                                interpolationAlgorithm: Option[String] = None,
                                interpolationDegree: Option[Int] = None,
                                forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                                backwardExtrapolationType: Option[String] = None, backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

    def this(referenceFrame: String, cartesian: Cartesian, interval: String) = this(Option(referenceFrame), Option(cartesian), None, None, None, Option(interval))

    def this(referenceFrame: String, x: Double, y: Double, z: Double, interval: String) = this(Option(referenceFrame), Option(new Cartesian(x, y, z)), None, None, None, Option(interval))

    def this(x: Double, y: Double, z: Double) = this(None, Option(new Cartesian(x, y, z)))

    def this(referenceFrame: String, x: Double, y: Double, z: Double) = this(Option(referenceFrame), Option(new Cartesian(x, y, z)))

    def this(cartographicDegrees: Cartographic) = this(cartographicDegrees = Option(cartographicDegrees))

    def this(lngLatAltT: LngLatAltT) = this(cartographicDegrees = Option(Cartographic(lngLatAltT)))
  }

  object CzmlPosition {
    implicit val fmt = Json.format[CzmlPosition]

    def apply(referenceFrame: String, cartesian: Cartesian, interval: String): CzmlPosition = new CzmlPosition(referenceFrame, cartesian, interval)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double, interval: String): CzmlPosition = new CzmlPosition(referenceFrame, x, y, z, interval)

    def apply(x: Double, y: Double, z: Double): CzmlPosition = new CzmlPosition(x, y, z)

    def apply(referenceFrame: String, x: Double, y: Double, z: Double): CzmlPosition = new CzmlPosition(referenceFrame, x, y, z)

    def apply(cartographicDegrees: Cartographic): CzmlPosition = new CzmlPosition(cartographicDegrees)

    def apply(lngLatAltT: LngLatAltT): CzmlPosition = new CzmlPosition(lngLatAltT)
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
          case None => JsSuccess(new CzmlPositions(Option(Array(JsPath.read[CzmlPosition].reads(js).getOrElse(CzmlPosition(None))))))
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
    * A non-timed value position
    */
  final case class Position(referenceFrame: Option[String] = None, cartesian: Option[Cartesian] = None,
                            cartographicRadians: Option[Array[Double]] = None,
                            cartographicDegrees: Option[Array[Double]] = None,
                            cartesianVelocity: Option[CartesianVelocity] = None,
                            interval: Option[String] = None,
                            references: Option[Array[String]] = None, epoch: Option[String] = None,
                            nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                            interpolationAlgorithm: Option[String] = None,
                            interpolationDegree: Option[Int] = None,
                            forwardExtrapolationType: Option[String] = None,
                            forwardExtrapolationDuration: Option[Double] = None,
                            backwardExtrapolationType: Option[String] = None,
                            backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

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
    * A property for an array of Position
    *
    * @param values the array of Position
    */
  final case class Positions(values: Option[Array[Position]]) {
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
            else Json.toJson(list.asInstanceOf[Array[Position]])
        }
      }
    }

    implicit val fmt: Format[Positions] = Format(theReads, theWrites)
  }

  final case class Repeat(cartesian2: Option[Array[Int]] = None, reference: Option[String] = None, epoch: Option[String] = None,
                          nextTime: Option[TimeValue] = None, previousTime: Option[TimeValue] = None,
                          interpolationAlgorithm: Option[String] = None,
                          interpolationDegree: Option[Int] = None,
                          forwardExtrapolationType: Option[String] = None, forwardExtrapolationDuration: Option[Double] = None,
                          backwardExtrapolationType: Option[String] = None, backwardExtrapolationDuration: Option[Double] = None) extends Interpolatable {

    def this(cartesian2: Array[Int]) = this(Option(cartesian2))

    def this(rx: Int, ry: Int) = this(Option(Array(rx, ry)))

  }

  object Repeat {
    implicit val fmt = Json.format[Repeat]

    def apply(cartesian2: Array[Int]): Repeat = new Repeat(cartesian2)

    def apply(rx: Int, ry: Int): Repeat = new Repeat(rx, ry)
  }

  //  "HORIZONTAL" or "VERTICAL"
  final case class StripeOrientation(stripeOrientation: Option[ArrayBuffer[Int]] = None, reference: Option[String] = None) {
    def this(stripeOrientation: ArrayBuffer[Int]) = this(Option(stripeOrientation))

    def this(stripeOrientation: Int) = this(Option(ArrayBuffer[Int](stripeOrientation)))
  }

  object StripeOrientation {
    implicit val fmt = Json.format[StripeOrientation]

    def apply(stripeOrientation: ArrayBuffer[Int]): StripeOrientation = new StripeOrientation(stripeOrientation)

    def apply(stripeOrientation: Int): StripeOrientation = new StripeOrientation(stripeOrientation)
  }

  final case class Stripe(orientation: Option[StripeOrientation] = None, evenColor: Option[ColorProperty] = None,
                          oddColor: Option[ColorProperty] = None, offset: Option[Number] = None,
                          repeat: Option[Repeat] = None) {

    def this(orientation: Int, evenColor: CzmlColor, oddColor: CzmlColor, offset: Double, repeat: Array[Int]) =
      this(Option(new StripeOrientation(orientation)), Option(new ColorProperty(evenColor)),
        Option(new ColorProperty(oddColor)), Option(new Number(offset)), Option(new Repeat(repeat)))

  }

  object Stripe {
    implicit val fmt = Json.format[Stripe]

    def apply(orientation: Int, evenColor: CzmlColor, oddColor: CzmlColor, offset: Double, repeat: Array[Int]): Stripe =
      new Stripe(orientation, evenColor, oddColor, offset, repeat)
  }

  final case class Grid(color: Option[ColorProperty] = None, cellAlpha: Option[Number] = None,
                        lineCount: Option[Repeat] = None, lineThickness: Option[Repeat] = None,
                        lineOffset: Option[Repeat] = None) {

    def this(color: CzmlColor, cellAlpha: Double, lineCount: Array[Int], lineThickness: Array[Int], lineOffset: Array[Int]) =
      this(Option(new ColorProperty(color)), Option(new Number(cellAlpha)),
        Option(new Repeat(lineCount)), Option(new Repeat(lineThickness)), Option(new Repeat(lineOffset)))
  }

  object Grid {
    implicit val fmt = Json.format[Grid]

    def apply(color: CzmlColor, cellAlpha: Double, lineCount: Array[Int], lineThickness: Array[Int], lineOffset: Array[Int]): Grid =
      new Grid(color, cellAlpha, lineCount, lineThickness, lineOffset)
  }

  final case class SolidColor(color: Option[ColorProperty] = None) {
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

  final case class Material(solidColor: Option[SolidColor] = None, image: Option[ImageUri] = None,
                            repeat: Option[Repeat] = None, grid: Option[Grid] = None,
                            stripe: Option[Stripe] = None) {

    def this(solidColor: ColorProperty) = this(solidColor = Option(SolidColor(solidColor)))

    def this(solidColor: CzmlColor) = this(solidColor = Option(SolidColor(solidColor)))

    def this(rgba: Rgba) = this(solidColor = Option(SolidColor(rgba)))

    def this(r: Int, g: Int, b: Int, a: Int) = this(solidColor = Option(SolidColor(r, g, b, a)))

    def this(rgbaf: Rgbaf) = this(solidColor = Option(SolidColor(rgbaf)))

    def this(r: Float, g: Float, b: Float, a: Float) = this(solidColor = Option(SolidColor(r, g, b, a)))

    def this(r: Double, g: Double, b: Double, a: Double) = this(solidColor = Option(SolidColor(r, g, b, a)))

    def this(uri: ImageUri) = this(image = Option(uri))

    def this(uri: String) = this(image = Option(ImageUri(uri)))

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

    def apply(uri: ImageUri): Material = new Material(uri)

    def apply(uri: String): Material = new Material(uri)

    def apply(color: javafx.scene.paint.Color): Material = new Material(color)

    def apply(color: java.awt.Color): Material = new Material(color)

  }

  final case class PolylineGlow(color: Option[ColorProperty] = None, glowPower: Option[Number] = None) {
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

  final case class PolylineOutline(color: Option[ColorProperty] = None, outlineColor: Option[ColorProperty] = None,
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
    */
  final case class LineMaterial(solidColor: Option[SolidColor] = None, polylineOutline: Option[PolylineOutline] = None,
                                polylineGlow: Option[PolylineGlow]) {

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
    * A path, which is a polyline defined by the motion of an object over time.
    * The possible vertices of the path are specified by the position property.
    */
  final case class Path(show: Option[Show] = None, material: Option[LineMaterial] = None,
                        width: Option[Number] = None, resolution: Option[Number] = None,
                        leadTime: Option[Number] = None, trailTime: Option[Number] = None) extends CzmlProperty {

    def this(material: LineMaterial, width: Number, resolution: Number) =
      this(Option(Show(true)), Option(material), Option(width), Option(resolution))

    def this(material: LineMaterial, width: Double, resolution: Double) =
      this(Option(Show(true)), Option(material), Option(new Number(width)), Option(new Number(resolution)))
  }

  object Path {
    implicit val fmt = Json.format[Path]

    def apply(material: LineMaterial, width: Number, resolution: Number): Path = new Path(material, width, resolution)

    def apply(material: LineMaterial, width: Double, resolution: Double): Path = new Path(material, width, resolution)

  }

  /**
    * A polyline, which is a line in the scene composed of multiple segments.
    */
  final case class Polyline(positions: Option[Positions] = None, show: Option[Show] = None,
                            material: Option[LineMaterial] = None, width: Option[Number] = None,
                            followSurface: Option[Boolean] = None) extends CzmlProperty {

    def this(positions: Positions, material: LineMaterial, width: Number, followSurface: Boolean) =
      this(Option(positions), Option(Show(true)), Option(material), Option(width), Option(followSurface))

    def this(positions: Position, material: LineMaterial, width: Double, followSurface: Boolean) =
      this(Option(new Positions(positions)), Option(Show(true)),
        Option(material), Option(new Number(width)), Option(followSurface))
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
  final case class Rectangle(coordinates: Option[WsenDegrees] = None, show: Option[Show] = None,
                             material: Option[Material] = None, height: Option[Number] = None,
                             extrudedHeight: Option[Number] = None, granularity: Option[Number] = None,
                             rotation: Option[Number] = None,
                             stRotation: Option[Number] = None, fill: Option[Boolean] = None,
                             outline: Option[Boolean] = None, outlineColor: Option[ColorProperty] = None,
                             outlineWidth: Option[Number] = None,
                             closeBottom: Option[Boolean] = None, closeTop: Option[Boolean] = None) extends CzmlProperty

  object Rectangle {
    implicit val fmt = Json.format[Rectangle]
  }

  /**
    * A wall
    */
  final case class Wall(positions: Option[Positions] = None, show: Option[Show] = None,
                        material: Option[Material] = None,
                        minimumHeights: Option[Array[Double]] = None,
                        maximumHeights: Option[Array[Double]] = None,
                        granularity: Option[Number] = None,
                        fill: Option[Boolean] = None,
                        outline: Option[Boolean] = None, outlineColor: Option[ColorProperty] = None,
                        outlineWidth: Option[Number] = None) extends CzmlProperty

  object Wall {
    implicit val fmt = Json.format[Wall]
  }

  /**
    * A polygon, which is a closed figure on the surface of the Earth.
    */
  final case class Polygon(positions: Option[Positions] = None, show: Option[Show] = None,
                           material: Option[Material] = None, height: Option[Number] = None,
                           extrudedHeight: Option[Number] = None, granularity: Option[Number] = None,
                           stRotation: Option[Number] = None, fill: Option[Boolean] = None,
                           outline: Option[Boolean] = None, outlineColor: Option[ColorProperty] = None,
                           perPositionHeight: Option[Boolean] = None) extends CzmlProperty {

    def this(positions: Positions, material: Material, height: Double, extrudedHeight: Double) =
      this(Option(positions), Option(Show(true)), Option(material), Option(Number(height)), Option(Number(extrudedHeight)))

    def this(positions: Position, material: Material, height: Double, extrudedHeight: Double) =
      this(Option(Positions(positions)), Option(Show(true)), Option(material), Option(Number(height)), Option(Number(extrudedHeight)))

  }

  object Polygon {
    implicit val fmt = Json.format[Polygon]

    def apply(positions: Positions, material: Material, height: Double, extrudedHeight: Double): Polygon =
      new Polygon(positions, material, height, extrudedHeight)

    def apply(positions: Position, material: Material, height: Double, extrudedHeight: Double): Polygon =
      new Polygon(positions, material, height, extrudedHeight)
  }

  /**
    * The dimensions of the ellipsoid. Also describes viewFrom property.
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
  final case class Ellipsoid(show: Option[Show] = None, radii: Option[Radii] = None,
                             fill: Option[Boolean] = None, material: Option[Material] = None,
                             outline: Option[Boolean] = None, outlineColor: Option[ColorProperty] = None,
                             stackPartitions: Option[Number] = None, slicePartitions: Option[Number] = None,
                             subdivisions: Option[Number] = None) extends CzmlProperty {

    def this(x: Double, y: Double, z: Double, fill: Boolean, material: Material,
             outline: Boolean, outlineColor: ColorProperty, stackPartitions: Double, slicePartitions: Double,
             subdivisions: Double) = this(Option(Show(true)), Option(Radii(x, y, z)), Option(fill),
      Option(material), Option(outline), Option(outlineColor), Option(Number(stackPartitions)),
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
    * A 3D model. The model is positioned and oriented using the position and orientation properties.
    */
  final case class Model(show: Option[Show] = None, scale: Option[Number] = None,
                         minimumPixelSize: Option[Number] = None, gltf: Option[ImageUri] = None) extends CzmlProperty {

    def this(scale: Double, minimumPixelSize: Double, gltf: String) =
      this(Option(Show(true)), Option(new Number(scale)),
        Option(new Number(minimumPixelSize)), Option(new ImageUri(gltf)))

  }

  object Model {
    implicit val fmt = Json.format[Model]

    def apply(scale: Double, minimumPixelSize: Double, gltf: String): Model = new Model(scale, minimumPixelSize, gltf)

  }

  /**
    * An ellipse, which is a closed curve on the surface of the Earth.
    * The ellipse is positioned using the position property.
    */
  final case class Ellipse(show: Option[Show] = None, semiMajorAxis: Option[Number] = None, semiMinorAxis: Option[Number] = None,
                           rotation: Option[Number] = None, material: Option[Material] = None,
                           height: Option[Number] = None, extrudedHeight: Option[Number] = None,
                           granularity: Option[Number] = None, stRotation: Option[Number] = None,
                           fill: Option[Boolean] = None, outline: Option[Boolean] = None, outlineColor: Option[ColorProperty] = None,
                           numberOfVerticalLines: Option[Number] = None) extends CzmlProperty {

    def this(semiMajorAxis: Double, semiMinorAxis: Double, rotation: Double,
             material: Material, height: Double, extrudedHeight: Double, granularity: Double,
             stRotation: Double, fill: Boolean, outline: Boolean, outlineColor: CzmlColor, numberOfVerticalLines: Double) =
      this(Option(Show(true)), Option(Number(semiMajorAxis)), Option(Number(semiMinorAxis)),
        Option(Number(rotation)), Option(material),
        Option(Number(height)), Option(Number(extrudedHeight)),
        Option(Number(granularity)), Option(Number(stRotation)), Option(fill),
        Option(outline), Option(ColorProperty(outlineColor)), Option(Number(numberOfVerticalLines)))

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
    * Indicates what part of a sensor should be displayed.
    *
    * @param portionToDisplay "COMPLETE", "BELOW_ELLIPSOID_HORIZON", "ABOVE_ELLIPSOID_HORIZON"
    * @param reference        A reference property
    */
  final case class PortionToDisplay(portionToDisplay: Option[String] = None, reference: Option[String] = None) {
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
    * A conical sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    */
  final case class ConicSensor(show: Option[Show] = None, innerHalfAngle: Option[Number] = None,
                               outerHalfAngle: Option[Number] = None, minimumClockAngle: Option[Number] = None,
                               maximumClockAngle: Option[Number] = None, radius: Option[Number] = None,
                               showIntersection: Option[Boolean] = None, intersectionColor: Option[ColorProperty] = None,
                               intersectionWidth: Option[Number] = None, showLateralSurfaces: Option[Boolean] = None,
                               lateralSurfaceMaterial: Option[Material] = None, showEllipsoidSurfaces: Option[Boolean] = None,
                               ellipsoidSurfaceMaterial: Option[Material] = None,
                               showEllipsoidHorizonSurfaces: Option[Boolean] = None,
                               ellipsoidHorizonSurfaceMaterial: Option[Material] = None,
                               showDomeSurfaces: Option[Boolean] = None, domeSurfaceMaterial: Option[Material] = None,
                               portionToDisplay: Option[PortionToDisplay] = None) extends CzmlProperty

  object ConicSensor {
    implicit val fmt = Json.format[ConicSensor]
  }

  /**
    * The list of directions defining the pyramid.
    */
  final case class Directions(spherical: Option[ArrayBuffer[Double]] = None, unitSpherical: Option[ArrayBuffer[Double]] = None,
                              cartesian: Option[ArrayBuffer[Double]] = None, unitCartesian: Option[ArrayBuffer[Double]] = None)

  object Directions {
    implicit val fmt = Json.format[Directions]
  }

  /**
    * A custom sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    */
  final case class CustomPatternSensor(show: Option[Show] = None, directions: Option[Directions] = None,
                                       radius: Option[Number] = None, showIntersection: Option[Boolean] = None,
                                       intersectionColor: Option[ColorProperty] = None, intersectionWidth: Option[Number] = None,
                                       showLateralSurfaces: Option[Boolean] = None, lateralSurfaceMaterial: Option[Material] = None,
                                       showEllipsoidHorizonSurfaces: Option[Boolean] = None, ellipsoidHorizonSurfaceMaterial: Option[Material] = None,
                                       showDomeSurfaces: Option[Boolean] = None, domeSurfaceMaterial: Option[Material] = None,
                                       portionToDisplay: Option[PortionToDisplay] = None) extends CzmlProperty

  object CustomPatternSensor {
    implicit val fmt = Json.format[CustomPatternSensor]
  }

  /**
    * Defines a fan, which starts at a point or apex and extends in a specified list of directions from the apex.
    * Each pair of directions forms a face of the fan extending to the specified radius.
    */
  final case class Fan(show: Option[Show] = None,
                       directions: Option[Directions] = None, radius: Option[Number] = None,
                       perDirectionRadius: Option[Boolean] = None, material: Option[Material] = None,
                       fill: Option[Boolean] = None, outline: Option[Boolean] = None,
                       numberOfRings: Option[Number] = None, outlineColor: Option[ColorProperty] = None) extends CzmlProperty

  object Fan {
    implicit val fmt = Json.format[Fan]
  }

  /**
    * A rectangular pyramid sensor volume taking into account occlusion of an ellipsoid, i.e., the globe.
    */
  final case class RectangularSensor(show: Option[Show] = None, xHalfAngle: Option[Number] = None,
                                     yHalfAngle: Option[Number] = None,
                                     radius: Option[Number] = None,
                                     showIntersection: Option[Boolean] = None,
                                     intersectionColor: Option[ColorProperty] = None,
                                     intersectionWidth: Option[Number] = None,
                                     showLateralSurfaces: Option[Boolean] = None,
                                     lateralSurfaceMaterial: Option[Material] = None,
                                     showEllipsoidSurfaces: Option[Boolean] = None,
                                     ellipsoidSurfaceMaterial: Option[Material] = None,
                                     showEllipsoidHorizonSurfaces: Option[Boolean] = None,
                                     ellipsoidHorizonSurfaceMaterial: Option[Material] = None,
                                     showDomeSurfaces: Option[Boolean] = None,
                                     domeSurfaceMaterial: Option[Material] = None,
                                     portionToDisplay: Option[PortionToDisplay] = None) extends CzmlProperty

  object RectangularSensor {
    implicit val fmt = Json.format[RectangularSensor]
  }

  /**
    * Defines a graphical vector that originates at the position property and
    * extends in the provided direction for the provided length.
    */
  final case class AgiVector(show: Option[Show] = None, color: Option[ColorProperty] = None,
                             direction: Option[Directions] = None, length: Option[Number] = None,
                             minimumLengthInPixels: Option[Number] = None) extends CzmlProperty {

    def this(show: Show, color: ColorProperty, direction: Directions, length: Number, minimumLengthInPixels: Number) =
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

    val propReads = new Reads[CZMLPacket] {
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

    val propWrites = new Writes[CZMLPacket] {
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

    implicit val fmt: Format[CZMLPacket] = Format(propReads, propWrites)
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
