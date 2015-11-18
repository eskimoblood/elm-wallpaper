module WallpaperGroup.Geom.GlideTranslate where

import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Geom.Util exposing (mapTransform)

glideTranslate' : (Point -> Point) -> Float -> Float -> Point -> Point
glideTranslate' mirrorFn offsetX offsetY point =
  let
    p = mirrorFn(point)
  in
    {
      x= p.x + offsetX,
      y= p.y + offsetY
     }

glideTranslate : (Point -> Point) -> Float -> Float -> (List Tile -> List Tile)
glideTranslate mirrorFn offsetX offsetY =  mapTransform (glideTranslate' mirrorFn offsetX offsetY)
