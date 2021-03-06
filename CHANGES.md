Change Log
==========

## changes in 0.6-SNAPSHOT


## changes in 0.5

* ready for cross publishing for scala_2.12.1
* remove the Logger
* added convenience Integer constructors to CzmlNumber and Number
* minor update to Util.writeJsToFile
* replaced UnitQuaternionValue in Orientation to UnitQuaternion

## changes in 0.4

* added color, colorBlendMode, and colorBlendAmount properties to Model
* added ColorBlendMode
* added ShadowMode
* added shadows to Box, Corridor, Cylinder, Ellipse, Ellipsoid, Polygon, Polyline, Rectangle, Wall, Model
* added silhouetteColor and silhouetteSize to Model

## changes in 0.3

   * restructured RADIAN and DEGREES as extending GEOTYPE
   * modified Cartographic and LngLatAlt to use GEOTYPE
   * added convenience constructors for CzmlPosition and CzmlPositions using GEOTYPE, similarly in CzmlImplicits
   * added HeightReference
   * added heightReference to Billboard, Label, Model and Point

## changes in 0.2

* Breaking changes
    * renamed LineMaterial to PolylineMaterial
    * renamed ImageUri to CzmlUri
    * renamed WsenDegrees to RectangleCoordinates in czmlCore and CzmlImplicits
    * changed CZMLPacket to take ViewFrom for field viewFrom instead of CzmlCartesian
    * changed Orientation to use UnitQuaternionValue
    * changed NodeTransformation to its own Rotation type instead of Orientation
    * renamed Stripe to StripeMaterial
    * renamed Grid to GridMaterial
    * renamed SolidColor to SolidColorMaterial
    * renamed Image to ImageMaterial
    * renamed Cartesian to Cartesian3D
    * renamed PolylineArrow to PolylineArrowMaterial
    * renamed PolylineGlow to PolylineGlowMaterial
    * renamed PolylineOutline to PolylineOutlineMaterial
    * renamed Style to LabelStyle

* other changes
    * added PolylineArrow to czmlCore
    * added polylineArrow, image, grid and stripe to PolylineMaterial.
    * added implicit PolylineArrowToPolylineArrowOp to CzmlImplicits
    * added Box to czmlProperties and its BoxDimensions to czmlCore
    * added CornerType and CornerTypeValue to czmlCore
    * added Cylinder to czmlProperties
    * added Corridor to czmlProperties
    * added UnitQuaternion and UnitQuaternionValue to czmlCore
    * added Rotation to czmlCore
    * added ViewFrom to czmlProperties
    * added UriInterval to CzmlUri
    * added NearFarScalar and NearFarScalarValue to CzmlCore
    * added width, height, scaleByDistance, translucencyByDistance, pixelOffsetScaleByDistance and imageSubRegion to Billboard
    * added translucencyByDistance, pixelOffsetScaleByDistance to Label
    * added scaleByDistance, translucencyByDistance to Point
    * added maximumScale to Model
    * added BoundingRectangle and BoundingRectangleValue to CzmlCore
    * added outlineWidth to Ellipsoid
    * added RectangleCoordValue and RectangleCoordValues to czmlCore
    * added wsen to RectangleCoordinates and made wsen and wsenDegrees Option[RectangleCoordValues]
    * added RectCoordToRectCoordValOp to CzmlImplicits
    * removed ArrayOfDoubleToRect from CzmlImplicits
