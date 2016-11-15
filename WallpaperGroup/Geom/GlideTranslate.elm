module WallpaperGroup.Geom.GlideTranslate exposing (..)

import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Geom.Util exposing (mapTransform)


glideTranslate_ : (Point -> Point) -> Float -> Float -> Point -> Point
glideTranslate_ mirrorFn offsetX offsetY point =
    let
        p =
            mirrorFn (point)
    in
        { x = p.x + offsetX
        , y = p.y + offsetY
        }


glideTranslate : (Point -> Point) -> Float -> Float -> List Tile -> List Tile
glideTranslate mirrorFn offsetX offsetY =
    mapTransform (glideTranslate_ mirrorFn offsetX offsetY)
