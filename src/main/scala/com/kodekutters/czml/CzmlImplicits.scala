package com.kodekutters.czml

import com.kodekutters.czml.czmlCore._
import com.kodekutters.czml.czmlProperties.CzmlCartesian
import scala.language.implicitConversions
import scala.reflect.runtime.universe._


/**
  * a set of implicit conversions. Use with care.
  *
  */
object CzmlImplicits {

  implicit def SeqOfCoordinateToCartesian(value: Seq[Coordinate]): Cartesian3D = new Cartesian3D(value)

  implicit def SeqOfCoordinate2DToCartesian2D(value: Seq[Coordinate2D]): Cartesian2D = new Cartesian2D(value)

  implicit def SeqOfLngLatAltToCartographic[T: TypeTag](value: Seq[LngLatAlt[T]]): Cartographic[T] = new Cartographic[T](value)

  implicit def StringToTimeInterval(value: String): TimeInterval = TimeInterval(value.trim)

  implicit def StringToTimeValue(value: String): TimeValue = new TimeValue(value)

  implicit def DoubleToTimeValue(value: Double): TimeValue = new TimeValue(value)

  implicit def BoolToCzmlBoolean(value: Boolean): CzmlBoolean = new CzmlBoolean(value)

  implicit def ColorToRgba(value: java.awt.Color): Rgba = new Rgba(value)

  implicit def ColorToRgbaf(value: javafx.scene.paint.Color): Rgbaf = new Rgbaf(value)

  implicit def DoubleToTimedDouble(value: Double): TimedDouble = new TimedDouble(value)

  implicit def StringToImageUri(value: String): CzmlUri = new CzmlUri(value)

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

  implicit def StringToStyle(value: String): LabelStyle = new LabelStyle(value)

  implicit def StringToStripeOrientation(value: String): StripeOrientation = new StripeOrientation(value)

  implicit def ColorToSolidColor(value: java.awt.Color): SolidColorMaterial = new SolidColorMaterial(value)

  implicit def StringToPortionToDisplay(value: String): PortionToDisplay = new PortionToDisplay(value)

  implicit def StringToDescription(value: String): Description = Description(value)

  implicit def StringToDescriptionOp(value: String): Option[Description] = Option(Description(value))

  // ------------------X to Option[X]-----------------------------------------------------------------------------------

  implicit def DescriptionToDescriptionOp(value: Description): Option[Description] = Option(value)

  implicit def CzmlCartesian2ToCzmlCartesian2Op(value: CzmlCartesian2): Option[CzmlCartesian2] = Option(value)

  implicit def NumberToNumberOp(value: Number): Option[Number] = Option(value)

//  implicit def DoubleToCzmlNumberOp(value: Double): Option[CzmlNumber] = Option(new CzmlNumber(value))

  implicit def ColorPropertyToColorPropertyOp(value: ColorProperty): Option[ColorProperty] = Option(value)

  implicit def FontToFontOp(value: Font): Option[Font] = Option(value)

  implicit def TextToTextOp(value: Text): Option[Text] = Option(value)

  implicit def StyleToStyleOp(value: LabelStyle): Option[LabelStyle] = Option(value)

  implicit def CzmlPositionToCzmlPositionOp(value: CzmlPosition): Option[CzmlPosition] = Option(value)

  implicit def StripeOrientationToStripeOrientationOp(value: StripeOrientation): Option[StripeOrientation] = Option(value)

  implicit def StripeToStripeOp(value: StripeMaterial): Option[StripeMaterial] = Option(value)

  implicit def GridToGridOp(value: GridMaterial): Option[GridMaterial] = Option(value)

  implicit def SolidColorToSolidColorOp(value: SolidColorMaterial): Option[SolidColorMaterial] = Option(value)

  implicit def MaterialToMaterialOp(value: Material): Option[Material] = Option(value)

  implicit def PolylineGlowToPolylineGlowOp(value: PolylineGlowMaterial): Option[PolylineGlowMaterial] = Option(value)

  implicit def PolylineArrowToPolylineArrowOp(value: PolylineArrowMaterial): Option[PolylineArrowMaterial] = Option(value)

  implicit def PolylineOutlineToPolylineOutlineOp(value: PolylineOutlineMaterial): Option[PolylineOutlineMaterial] = Option(value)

  implicit def LineMaterialToLineMaterialOp(value: PolylineMaterial): Option[PolylineMaterial] = Option(value)

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

  implicit def ImageUriToImageUriOp(value: CzmlUri): Option[CzmlUri] = Option(value)

  implicit def CartesianToCartesianOp(value: Cartesian3D): Option[Cartesian3D] = Option(value)

  implicit def Cartesian2DToCartesian2DOp(value: Cartesian2D): Option[Cartesian2D] = Option(value)

  implicit def CartographicToCartographicOp[T: TypeTag](value: Cartographic[T]): Option[Cartographic[T]] = Option(value)

  implicit def CartesianVelocityToCartesianVelocityOp(value: CartesianVelocity): Option[CartesianVelocity] = Option(value)

  implicit def RgbaListToRgbaListOp(value: RgbaList): Option[RgbaList] = Option(value)

  implicit def RgbafListToRgbafListOp(value: RgbafList): Option[RgbafList] = Option(value)

  implicit def OriginToOriginOp[T: TypeTag](value: Origin[T]): Option[Origin[T]] = Option(value)

  implicit def PositionsToPositionsOp(value: Positions): Option[Positions] = Option(value)

  implicit def RectCoordToRectCoordOp(value: RectangleCoordinates): Option[RectangleCoordinates] = Option(value)

  implicit def RectCoordToRectCoordValOp(value: RectangleCoordValues): Option[RectangleCoordValues] = Option(value)

  // ------------------------------------------------------------------------------------------------------

  implicit def DoubleToNumberOp(value: Double): Option[Number] = Option(Number(value))

  implicit def StringToImageUriOp(value: String): Option[CzmlUri] = Option(new CzmlUri(value))

  implicit def StringToTextOp(value: String): Option[Text] = Option(new Text(value))

  implicit def StringToFontOp(value: String): Option[Font] = Option(new Font(value))

  implicit def StringToStyleOp(value: String): Option[LabelStyle] = Option(new LabelStyle(value))

  implicit def ColorToSolidColorOp(value: java.awt.Color): Option[SolidColorMaterial] = Option(new SolidColorMaterial(value))

  implicit def ColorToCzmlColorOp(value: java.awt.Color): Option[CzmlColor] = Option(new CzmlColor(value))

  implicit def ColorToColorPropertyOp(value: java.awt.Color): Option[ColorProperty] = Option(new ColorProperty(value))

  implicit def StringToPortionToDisplayOp(value: String): Option[PortionToDisplay] = Option(PortionToDisplay(value))

  implicit def StringToStripeOrientationOp(value: String): Option[StripeOrientation] = Option(new StripeOrientation(value))

  // ------------------------------------------------------------------------------------------------------

  implicit def CzmlCartesianToCzmlCartesianOp(value: CzmlCartesian): Option[CzmlCartesian] = Option(value)

  implicit def ArrayOfPositionToPositionsOp(value: Array[com.kodekutters.czml.czmlCore.Position]): Option[Positions] = Option(new Positions(value))

}
