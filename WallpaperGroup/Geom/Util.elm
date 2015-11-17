module WallpaperGroup.Geom.Util where


import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Tile exposing (Tile)
import List.Extra exposing (last)
import List as L

mapOverPoints : (Point -> Point) -> Line -> Line
mapOverPoints transformFunc line =  L.map transformFunc line


mapTransform : (Point -> Point) -> Tile -> Tile
mapTransform transformFunc tile =
  let
    line = L.head tile
  in
    case line of
      Just l -> (L.map  transformFunc l) :: tile
      Nothing -> tile


split : Point -> Point -> Float -> Point
split start end percentage =
  {
    x= start.x + (end.x - start.x) * percentage,
    y= start.y + (end.y - start.y) * percentage
  }


rectCoords : Float -> Float -> Line
rectCoords w h = [
  {x= 0, y= 0},
  {x= w, y= 0},
  {x= w, y= h},
  {x= 0, y= h}]


triangleCoords : Float -> Float -> Line
triangleCoords w h = [
  {x= w / 2, y= 0},
  {x= w, y= h},
  {x= 0, y= h}]


rightTriangleCoords : Float -> Float -> Line
rightTriangleCoords w h =[
  {x= w, y= 0},
  {x= w, y= h},
  {x= 0, y= h}]
