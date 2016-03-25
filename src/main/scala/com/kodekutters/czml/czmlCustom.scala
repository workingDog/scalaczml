package com.kodekutters.czml

import com.kodekutters.czml.czmlCore._
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.collection.immutable._


/**
  * This package implements CustomProperty and its support elements for the CZML custom properties
  *
  * see:  https://github.com/AnalyticalGraphicsInc/cesium/issues/3162
  *
  */
package object czmlCustom {

  // all CustomProperty must extend this trait
  sealed trait CustomProperty

  /**
    * convenience method to write as json a primitive value or collection of such
    *
    * @param s the primitive value to write as json
    */
  private[this] def basicWrite(s: Any): JsValue = {
    s match {
      case z: String => JsString(z)
      case z: Int => JsNumber(z)
      case z: Long => JsNumber(z)
      case z: Double => JsNumber(z)
      case z: Float => JsNumber(z.toDouble)
      case z: Byte => JsNumber(z.toInt)
      case z: Short => JsNumber(z.toInt)
      case z: Boolean => JsBoolean(z)
      case z: BigDecimal => JsNumber(z)
      case z: scala.collection.mutable.Traversable[_] => JsArray(for (s <- z.toSeq) yield basicWrite(s))
      case z: scala.collection.immutable.Traversable[_] => JsArray(for (s <- z.toSeq) yield basicWrite(s))
      case z => JsNull
    }
  }

  /**
    * convenience method to read a json value into a primitive value
    *
    * @param s the json value to read
    */
  private[this] def basicRead(s: JsValue): Any = {
    s match {
      case JsString(z) => z
      case JsNumber(z) => z.toDouble
      case JsBoolean(z) => z
      case JsArray(arr) => for (e <- arr) yield basicRead(e) // recursion
      case _ => null // includes the case JsNull
    }
  }

  /**
    * a custom property for a time interval and associated value
    *
    * @param interval the time interval either a string or a double
    * @param value    the simple value
    */
  case class CustomInterval(interval: TimeValue, value: Any) extends CustomProperty

  object CustomInterval {

    val theReads = new Reads[CustomInterval] {
      def reads(js: JsValue): JsResult[CustomInterval] = {
        val result = for {
          time <- (JsPath \ "interval").read[TimeValue].reads(js).asOpt
          value <- Option(basicRead((js \ "value").getOrElse(JsNull)))
        } yield CustomInterval(time, value)
        result.map(r => JsSuccess(r)).getOrElse(JsError("could not read as CustomInterval: " + js.toString))
      }
    }

    val theWrites = new Writes[CustomInterval] {
      def writes(x: CustomInterval) = Json.obj(
        "interval" -> TimeValue.fmt.writes(x.interval),
        "value" -> basicWrite(x.value))
    }

    implicit val fmt: Format[CustomInterval] = Format(theReads, theWrites)
  }

  /**
    * a custom property for a basic/primitive value or array of such (recursively)
    *
    */
  case class CustomBasic[T](value: T) extends CustomProperty

  object CustomBasic {
    implicit def theReads[T](implicit fmt: Reads[T]) = new Reads[CustomBasic[T]] {
      def reads(js: JsValue): JsResult[CustomBasic[T]] =
        basicRead(js) match {
          case x if x != null => JsSuccess(new CustomBasic[T](x.asInstanceOf[T]))
          case _ => JsError("Could not read CustomBasic " + js.toString)
        }
    }

    implicit def theWrites[T](implicit fmt: Writes[T]) = new Writes[CustomBasic[T]] {
      def writes(x: CustomBasic[T]) = basicWrite(x.value)
    }
  }

  /**
    * a custom property for a List with a value field, e.g. "a-name": { "value": [1,2,3] }
    *
    * @param value the List of values
    */
  case class CustomList(value: List[Any]) extends CustomProperty

  object CustomList {
    val theReads = new Reads[CustomList] {
      def reads(js: JsValue): JsResult[CustomList] = {
        (js \ "value").getOrElse(JsNull) match {
          case JsArray(arr) =>
            val theList = for (e <- arr) yield basicRead(e)
            JsSuccess(new CustomList(theList.toList))
          case x => JsError("Could not read CustomArray " + js.toString)
        }
      }
    }

    val theWrites = new Writes[CustomList] {
      def writes(x: CustomList) = Json.obj("value" -> basicWrite(x.value))
    }

    implicit val fmt: Format[CustomList] = Format(theReads, theWrites)
  }

  /**
    * the generic custom property object
    * provides for reading and writing json format
    *
    * Currently there are 4 types of CustomProperty implemented:
    *
    * - CustomBasic for String,Int,Double...Array of such recursively
    * - CustomList for a List/Array with a "value" field
    * - CustomInterval for value and time interval
    * - CustomMap for map of (key,value)
    */
  object CustomProperty {

    val theReads = new Reads[CustomProperty] {
      def reads(js: JsValue): JsResult[CustomProperty] = {
        CustomBasic.theReads.reads(js) |
          CustomInterval.theReads.reads(js) |
          CustomList.theReads.reads(js) |
          CustomMap.theReads.reads(js)
      }
    }

    val theWrites = new Writes[CustomProperty] {
      def writes(cust: CustomProperty) = {
        cust match {
          case s: CustomBasic[_] => CustomBasic.theWrites.writes(s.asInstanceOf[CustomBasic[CustomProperty]])
          case s: CustomInterval => CustomInterval.fmt.writes(s)
          case s: CustomList => CustomList.fmt.writes(s)
          case s: CustomMap => CustomMap.fmt.writes(s)
          case _ => JsNull
        }
      }
    }

    implicit val fmt: Format[CustomProperty] = Format(theReads, theWrites)
  }

  /**
    * a custom property map with key=the field name, and value=any object
    */
  case class CustomMap(value: Map[String, Any]) extends CustomProperty

  object CustomMap {

    def apply() = new CustomMap(Map.empty)

    /**
      * reads the fields of the JsObject into a map (key=field name, value=any)
      *
      * @param js the input json object
      * @return a map (key=field name, value=any)
      */
    private[this] def readFields(js: JsObject): Map[String, Any] = {
      js.fields.collect {
        case (key, JsBoolean(value)) => key -> value
        case (key, JsString(value)) => key -> value
        case (key, JsNumber(value)) => key -> value
        case (key, JsObject(value)) => key -> readFields(JsObject(value)) // recursion
        case (key, JsArray(value)) => key -> readArray(JsArray(value))
      }.toMap
    }

    private[this] def readArray(js: JsArray): Any = {
      js match {
        case JsArray(value) =>
          Json.fromJson[Array[CustomInterval]](JsArray(value)).asOpt match {
            case Some(x) => x
            case None =>
              for (e <- value) yield {
                e match {
                  case JsObject(v) => readFields(JsObject(v))
                  case JsArray(v) => readArray(JsArray(v)) // recursion
                  case x => basicRead(x)
                }
              }
            case _ => null
          }
      }
    }

    val theReads = new Reads[CustomMap] {
      def reads(json: JsValue): JsResult[CustomMap] = {
        json match {
          case x: JsObject => JsSuccess(new CustomMap(readFields(x)))
          case x: JsArray => JsSuccess(new CustomMap(Map("value" -> readArray(x))))
          case x: JsValue => JsSuccess(new CustomMap(Map("value" -> basicRead(x))))
          case x =>
            logger.error("could not read CustomProperty: " + x.toString())
            JsError(s"Could not read CustomProperty : $x")
        }
      }
    }

    private[this] def writeTraversable(theList: Traversable[Any]): JsValue = {
      JsArray(for (e <- theList.toList) yield {
        e match {
          case p: Map[_, _] => writeMap(p.asInstanceOf[Map[String, Any]])
          case p: Traversable[_] => writeTraversable(p)
          case x => basicWrite(x)
        }
      })
    }

    private[this] def writeMap(theMap: Map[String, Any]): JsValue = {
      val list: Map[String, JsValue] = for ((k, v) <- theMap) yield {
        v match {
          case x: Map[_, _] => k -> writeMap(x.asInstanceOf[Map[String, Any]])
          case x: scala.collection.mutable.Traversable[_] => k -> writeTraversable(x.toList)
          case x: scala.collection.immutable.Traversable[_] => k -> writeTraversable(x)
          case x => k -> basicWrite(x)
        }
      }
      JsObject(list)
    }

    val theWrites = new Writes[CustomMap] {
      def writes(custom: CustomMap) = writeMap(custom.value)
    }

    implicit val fmt: Format[CustomMap] = Format(theReads, theWrites)
  }

}
