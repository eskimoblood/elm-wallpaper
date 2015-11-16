module WallpaperGroup.Pattern where


import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Group exposing (Group)


calcStep :  (List Tile -> List Tile) -> List Tile -> List Tile
calcStep func lines = func lines


calculateTile : Tile -> List (List Tile -> List Tile) ->  Tile
calculateTile lines steps = List.concat (List.foldl  calcStep [lines] steps)


translateTile : Point -> Line -> Line
translateTile transition line=  List.map (add transition) line


translate : Point -> Tile -> Tile
translate  transition tile = List.map (translateTile transition) tile


pattern : Group -> Int -> Int -> Tile -> List Tile
pattern group columns rows lines =
  let tile = List.concat (calculateTile lines group.steps);
      numberOfTiles = columns * rows
  in
    [0..numberOfTiles]
      |> List.map group.translate
      |> List.map translate tile
