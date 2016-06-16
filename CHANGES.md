Change Log
==========

## changes in 0.2-SNAPSHOT

* Breaking changes
    * renamed LineMaterial to PolylineMaterial

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
    * changed Orientation to use UnitQuaternionValue
    * changed NodeTransformation to its own Rotation type instead of Orientation
    * added ViewFrom to czmlProperties
    * changed CZMLPacket to take ViewFrom for field viewFrom instead of CzmlCartesian
    * renamed WsenDegrees to RectangleCoordinates in czmlCore and CzmlImplicits
    *


