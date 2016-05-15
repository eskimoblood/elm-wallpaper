module WallpaperGroup.Geom.Mirror exposing (..)

import WallpaperGroup.Geom.Point exposing (Point, subtract)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Geom.Util exposing (mapTransform)


type alias Axis =
    { p1 : Point, p2 : Point }


mirror : Axis -> Point -> Point
mirror { p1, p2 } point =
    let
        d = subtract p2 p1

        e = subtract point p1

        a = (d.x * d.x - d.y * d.y) / (d.x * d.x + d.y * d.y)

        b = 2 * d.x * d.y / (d.x * d.x + d.y * d.y)
    in
        { x = a * e.x + b * e.y + p1.x
        , y = b * e.x - a * e.y + p1.y
        }


mirrorHorizontal : Float -> Float -> List Tile -> List Tile
mirrorHorizontal w h =
    mapTransform (mirror { p1 = { x = 0, y = w }, p2 = { x = 2 * w, y = h } })


mirrorVertical : Float -> Float -> List Tile -> List Tile
mirrorVertical w h =
    mapTransform (mirror { p1 = { x = w, y = 0 }, p2 = { x = w, y = 2 * h } })


mirrorDiagonalRL : Float -> Float -> List Tile -> List Tile
mirrorDiagonalRL w h =
    mapTransform (mirror { p1 = { x = 0, y = 0 }, p2 = { x = w, y = h } })


mirrorDiagonalLR : Float -> Float -> List Tile -> List Tile
mirrorDiagonalLR w h =
    mapTransform (mirror { p1 = { x = w, y = 0 }, p2 = { x = 0, y = h } })


mirrorHex : Float -> List Tile -> List Tile
mirrorHex w =
    mapTransform (mirror { p1 = { x = (sqrt 3) / 2 * w, y = w }, p2 = { x = 0, y = w * 0.5 } })


mirrorTriangle : Float -> Float -> List Tile -> List Tile
mirrorTriangle w h =
    mapTransform (mirror { p1 = { x = w / 2, y = 0 }, p2 = { x = w, y = h } })
