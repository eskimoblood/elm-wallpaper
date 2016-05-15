module WallpaperGroup.Geom.Util exposing  (..) 

import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Geom.BoundingBox exposing (..)
import List as L


mapOverPoints : (Point -> Point) -> Line -> Line
mapOverPoints transformFunc line =
    L.map transformFunc line


mapTransform : (Point -> Point) -> List Tile -> List Tile
mapTransform transformFunc tiles =
    let
        tile = L.head tiles
    in
        case tile of
            Just t ->
                (L.map (mapOverPoints transformFunc) t) :: tiles

            Nothing ->
                tiles


split : Point -> Point -> Float -> Point
split start end percentage =
    { x = start.x + (end.x - start.x) * percentage
    , y = start.y + (end.y - start.y) * percentage
    }


rectCoords : Float -> Float -> BoundingBox
rectCoords w h =
    Rect { x = 0, y = 0 } { x = w, y = 0 } { x = w, y = h } { x = 0, y = h }


triangleCoords : Float -> Float -> BoundingBox
triangleCoords w h =
    Triangle { x = w / 2, y = 0 } { x = w, y = h } { x = 0, y = h }


rightTriangleCoords : Float -> Float -> BoundingBox
rightTriangleCoords w h =
    Triangle { x = w, y = 0 } { x = w, y = h } { x = 0, y = h }
