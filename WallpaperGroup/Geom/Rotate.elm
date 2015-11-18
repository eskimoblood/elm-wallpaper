module WallpaperGroup.Geom.Rotate where


import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Geom.Util exposing (mapTransform)


rotate : Float -> Point -> Point -> Point
rotate theta center p =
  let angleInRadians = theta * pi / 180
      sinTheta = sin(angleInRadians)
      cosTheta = cos(angleInRadians)
      oX = p.x - center.x
      oY = p.y - center.y
  in
     {
       x= cosTheta * oX - sinTheta * oY + center.x,
       y= sinTheta * oX + cosTheta * oY + center.y
     }


rotate90 : Point -> (List Tile -> List Tile)
rotate90 center = mapTransform (rotate 90 center)


rotate120 : Point -> (List Tile -> List Tile)
rotate120 center = mapTransform (rotate 120 center)


rotate180 : Point -> (List Tile -> List Tile)
rotate180 center = mapTransform (rotate 180 center)
