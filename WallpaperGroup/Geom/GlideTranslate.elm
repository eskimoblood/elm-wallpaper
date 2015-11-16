module WallpaperGroup.Geom.GlideTranslate where

import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Util exposing (mapTransform)

_glideTranslate : (Point -> Point) -> Float -> Float -> Point -> Point
_glideTranslate mirrorFn offsetX offsetY point =
  let
    p = mirrorFn(point)
  in
    {
      x= p.x + offsetX,
      y= p.y + offsetY
     }

glideTranslate : (Point -> Point) -> Float -> Float -> (List Line -> List Line)
glideTranslate mirrorFn offsetX offsetY =  mapTransform (_glideTranslate mirrorFn offsetX offsetY)
