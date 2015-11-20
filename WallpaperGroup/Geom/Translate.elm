module WallpaperGroup.Geom.Translate where


import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Util exposing (mapTransform)


w : Float -> Int -> Int -> Float
w width i columns = width * toFloat(i % columns)


h : Float -> Int -> Int -> Float
h height i columns =  height * toFloat (i // columns)


translate : Float -> Float -> Float  -> Float  -> Int -> Int -> Point
translate fW fH width height columns i =
  {
    x = fW * w width i columns,
    y = fH * h height i columns
  }


translateShifted :  Float -> Float -> Float -> Int -> Int -> Point
translateShifted fH width height columns i =
  let
    offsetX = if  (i // columns) % 2 == 0 then
                width / 2
              else
                0
  in
    {
      x= offsetX + w width i columns,
      y= fH * h height i columns
    }


w1h1 : (Float  -> Float  -> Int -> Int -> Point)
w1h1 = translate 1 1


w1h2 : (Float  -> Float  -> Int -> Int -> Point)
w1h2 = translate 1 2


w2h1 : (Float  -> Float  -> Int -> Int -> Point)
w2h1 = translate 2 1


w2h2 : (Float  -> Float  -> Int -> Int -> Point)
w2h2 = translate 2 2


shifted : (Float  -> Float  -> Int -> Int -> Point)
shifted = translateShifted 1


hex : (Float  -> Float  -> Int -> Int -> Point)
hex = translateShifted 0.75
