module WallpaperGroup.Geom.Translate where


import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Util exposing (mapTransform)


w : number -> Int -> Int -> Float
w width i columns = width * toFloat(i % columns)


h : number -> Int -> Int -> Float
h height i columns =  height * toFloat (i // columns)


translate : number -> number -> number  -> number  -> Int -> Int -> Point
translate fW fH width height columns i =
  {
    x = fW * w width i columns,
    y = fH * h height i columns
  }


_translateShifted :  number -> number -> number -> Int -> Int -> Point
_translateShifted fH width height columns i =
  let
    offsetX = if | (i // columns) % 2 == 0 ->  width / 2
                 | otherwise -> 0
  in
    {
      x= offsetX + w width i columns,
      y= fH * h height i columns
    }


w1h1 : (number  -> number  -> Int -> Int -> Point)
w1h1 = translate 1 1


w1h2 : (number  -> number  -> Int -> Int -> Point)
w1h2 = translate 1 2


w2h1 : (number  -> number  -> Int -> Int -> Point)
w2h1 = translate 2 1


w2h2 : (number  -> number  -> Int -> Int -> Point)
w2h2 = translate 2 2


shifted : (number  -> number  -> Int -> Int -> Point)
shifted = _translateShifted 1


hex : (number  -> number  -> Int -> Int -> Point)
hex = _translateShifted 0.75
