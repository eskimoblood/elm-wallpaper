module WallpaperGroup.Geom.Mirror where

import WallpaperGroup.Geom.Point
import WallpaperGroup.Geom.Line
import WallpaperGroup.Geom.Util exposing (mapTransform)

mirror : Line -> Point -> Point
mirror {p1, p2} point =
  let d = subtract p2 p1
      e = subtract point p1
      a = (d.x * d.x - d.y * d.y) / (d.x * d.x + d.y * d.y)
      b = 2 * d.x * d.y / (d.x * d.x + d.y * d.y)
  in
      {
        x = a * e.x + b * e.y + p1.x,
        y = b * e.x - a * e.y + p1.y
      }

mirrorHorizontal : Float -> Float -> (List Line -> List Line)
mirrorHorizontal w h = mapTransform (mirror [{x= 0, y= w}, {x= 2 * w, y= h}])

mirrorVertical: Float -> Float -> (List Line -> List Line)
mirrorVertical w h = mapTransform (mirror [{x= w, y= 0}, {x= w, y= 2 * h}])

mirrorDiagonalRL: Float -> Float -> (List Line -> List Line)
mirrorDiagonalRL w h = mapTransform (mirror [{x= 0, y= 0}, {x= w, y= h}])

mirrorDiagonalLR: Float -> Float -> (List Line -> List Line)
mirrorDiagonalLR w h = mapTransform (mirror([{x= w, y= 0}, {x= 0, y= h}])

mirrorHex : Float -> (List Line -> List Line)
mirrorHex w = mapTransform (mirror[{x= Math.sqrt(3) / 2 * w, y= w}, {x= 0, y= w * 0.5}])

mirrorDiagonalLR: Float -> Float -> (List Line -> List Line)
mirrorTriangle w h = mapTransform (mirror [{x= w / 2, y= 0}, {x= w, y= h}])
