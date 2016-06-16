Change Log
==========

## changes in 0.2-SNAPSHOT

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





