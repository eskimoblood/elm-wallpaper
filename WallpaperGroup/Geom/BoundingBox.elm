module WallpaperGroup.Geom.BoundingBox where

{-|
Bounding box of a single pattern.

@docs BoundingBox

-}

{-|
  Bounding box can be a triangle oder a rect.

-}
type BoundingBox
  = Triangle {x: Float, y: Float} {x: Float, y: Float} {x: Float, y: Float}
  | Rect {x: Float, y: Float} {x: Float, y: Float} {x: Float, y: Float} {x: Float, y: Float}
