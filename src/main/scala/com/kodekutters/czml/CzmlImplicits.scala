package com.kodekutters.czml

import com.kodekutters.czml.czmlCore._
import com.kodekutters.czml.czmlProperties.CzmlCartesian
import scala.language.implicitConversions
import scala.reflect.runtime.universe._


/**
  * a set of implicit conversions for CZML
  *
  */
object CzmlImplicits {

  implicit def ArrayOfDoubleToWsenDegrees(value: Array[Double]): WsenDegrees = new WsenDegrees(value)

  implicit def SeqOfCoordinateToCartesian(value: Seq[Coordinate]): Cartesian = new Cartesian(value)

  implicit def SeqOfCoordinate2DToCartesian2D(value: Seq[Coordinate2D]): Cartesian2D = new Cartesian2D(value)

  implicit def SeqOfLngLatAltToCartographic[T: TypeTag](value: Seq[LngLatAlt[T]]): Cartographic[T] = new Cartographic[T](value)

  implicit def StringToTimeInterval(value: String): TimeInterval = TimeInterval(value.trim)

  implicit def StringToTimeValue(value: String): TimeValue = new TimeValue(value)

  implicit def DoubleToTimeValue(value: Double): TimeValue = new TimeValue(value)

  implicit def BoolToCzmlBoolean(value: Boolean): CzmlBoolean = new CzmlBoolean(value)

  implicit def ColorToRgba(value: java.awt.Color): Rgba = new Rgba(value)

  implicit def ColorToRgbaf(value: javafx.scene.paint.Color): Rgbaf = new Rgbaf(value)

  implicit def DoubleToTimedDouble(value: Double): TimedDouble = new TimedDouble(value)

  implicit def StringToImageUri(value: String): ImageUri = new ImageUri(value)

  implicit def Cartesian2DToCzmlCartesian2(value: Cartesian2D): CzmlCartesian2 = new CzmlCartesian2(value)

  implicit def DoubleToCzmlNumber(value: Double): CzmlNumber = new CzmlNumber(value)

  implicit def TimedNumbersToCzmlNumber(value: TimedNumbers): CzmlNumber = new CzmlNumber(value)

  implicit def ArrayOfDoubleToCzmlNumber(value: Array[Double]): CzmlNumber = new CzmlNumber(Option(value))

  implicit def DoubleToNumber(value: Double): Number = new Number(value)

  implicit def ColorToCzmlColor(value: java.awt.Color): CzmlColor = new CzmlColor(value)

  implicit def ColorToCzmlColor(value: javafx.scene.paint.Color): CzmlColor = new CzmlColor(value)

  implicit def ColorToColorProperty(value: java.awt.Color): ColorProperty = new ColorProperty(value)

  implicit def ColorToColorProperty(value: javafx.scene.paint.Color): ColorProperty = new ColorProperty(value)

  implicit def StringToFont(value: String): Font = new Font(value)

  implicit def StringToText(value: String): Text = new Text(value)

  implicit def StringToStyle(value: String): Style = new Style(value)

  implicit def StringToStripeOrientation(value: String): StripeOrientation = new StripeOrientation(value)

  implicit def ColorToSolidColor(value: java.awt.Color): SolidColor = new SolidColor(value)

  implicit def StringToPortionToDisplay(value: String): PortionToDisplay = new PortionToDisplay(value)

  // ------------------X to Option[X]-----------------------------------------------------------------------------------

  implicit def CzmlCartesian2ToCzmlCartesian2Op(value: CzmlCartesian2): Option[CzmlCartesian2] = Option(value)

  implicit def NumberToNumberOp(value: Number): Option[Number] = Option(value)

  implicit def ColorPropertyToColorPropertyOp(value: ColorProperty): Option[ColorProperty] = Option(value)

  implicit def FontToFontOp(value: Font): Option[Font] = Option(value)

  implicit def TextToTextOp(value: Text): Option[Text] = Option(value)

  implicit def StyleToStyleOp(value: Style): Option[Style] = Option(value)

  implicit def CzmlPositionToCzmlPositionOp(value: CzmlPosition): Option[CzmlPosition] = Option(value)

  implicit def StripeOrientationToStripeOrientationOp(value: StripeOrientation): Option[StripeOrientation] = Option(value)

  implicit def StripeToStripeOp(value: Stripe): Option[Stripe] = Option(value)

  implicit def GridToGridOp(value: Grid): Option[Grid] = Option(value)

  implicit def SolidColorToSolidColorOp(value: SolidColor): Option[SolidColor] = Option(value)

  implicit def MaterialToMaterialOp(value: Material): Option[Material] = Option(value)

  implicit def PolylineGlowToPolylineGlowOp(value: PolylineGlow): Option[PolylineGlow] = Option(value)

  implicit def PolylineOutlineToPolylineOutlineOp(value: PolylineOutline): Option[PolylineOutline] = Option(value)

  implicit def LineMaterialToLineMaterialOp(value: LineMaterial): Option[LineMaterial] = Option(value)

  implicit def PortionToDisplayToPortionToDisplayOp(value: PortionToDisplay): Option[PortionToDisplay] = Option(value)

  implicit def DirectionsToDirectionsOp(value: Directions): Option[Directions] = Option(value)

  implicit def InterpolatableToInterpolatableOp(value: Interpolatable): Option[Interpolatable] = Option(value)

  implicit def ArrDoubleToArrDoubleOp(value: Array[Double]): Option[Array[Double]] = Option(value)

  implicit def StringToTimeValueOp(value: String): Option[TimeValue] = Option(new TimeValue(value))

  implicit def StringToStringOp(value: String): Option[String] = Option(value)

  implicit def DoubleToDoubleOp(value: Double): Option[Double] = Option(value)

  implicit def IntToIntOp(value: Int): Option[Int] = Option(value)

  implicit def BoolToBoolOp(value: Boolean): Option[Boolean] = Option(value)

  implicit def BoolToCzmlBooleanOp(value: Boolean): Option[CzmlBoolean] = Option(new CzmlBoolean(value))

  implicit def NodeTransformationsToNodeTransformationsOp(value: NodeTransformations): Option[NodeTransformations] = Option(value)

  implicit def ImageUriToImageUriOp(value: ImageUri): Option[ImageUri] = Option(value)

  implicit def CartesianToCartesianOp(value: Cartesian): Option[Cartesian] = Option(value)

  implicit def Cartesian2DToCartesian2DOp(value: Cartesian2D): Option[Cartesian2D] = Option(value)

  implicit def CartographicToCartographicOp[T: TypeTag](value: Cartographic[T]): Option[Cartographic[T]] = Option(value)

  implicit def CartesianVelocityToCartesianVelocityOp(value: CartesianVelocity): Option[CartesianVelocity] = Option(value)

  implicit def RgbaListToRgbaListOp(value: RgbaList): Option[RgbaList] = Option(value)

  implicit def RgbafListToRgbafListOp(value: RgbafList): Option[RgbafList] = Option(value)

  implicit def OriginToOriginOp[T: TypeTag](value: Origin[T]): Option[Origin[T]] = Option(value)

  implicit def PositionsToPositionsOp(value: Positions): Option[Positions] = Option(value)

  implicit def WsenDegreesToWsenDegreesOp(value: WsenDegrees): Option[WsenDegrees] = Option(value)



  // ------------------------------------------------------------------------------------------------------

  implicit def CzmlCartesianToCzmlCartesianOp(value: CzmlCartesian): Option[CzmlCartesian] = Option(value)


}