module WallpaperGroup.Group exposing (..)

{-|
The predefined Groups according to
[this](https://en.wikipedia.org/wiki/Wallpaper_group) Wikipedia entry

@docs Group
-}


{-|
Create a pattern width the specific width and/or height.
For triangle shaped pattern only the width of the base line is needed.
-}
type Group
    = P1 Float Float
    | P2 Float Float
    | Pm Float Float
    | Pg Float Float
    | Cm Float Float
    | P2mm Float Float
    | P2mg Float Float
    | P2gg Float Float
    | C2mm Float Float
    | P4 Float Float
    | P4mm Float Float
    | P4mg Float Float
    | P3 Float
    | P3m1 Float
    | P31m Float
    | P6 Float
